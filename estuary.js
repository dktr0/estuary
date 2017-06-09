"use strict";
process.title = 'estuary';
var stderr = process.stderr;

// dependencies
var http = require('http');
var url = require('url');
var WebSocket = require('ws');
var express = require('express');
var nopt = require('nopt');
var fs = require('fs');

// parse command-line options
var knownOpts = {
    "password" : [String, null],
    "tcp-port" : [Number, null],
    "immortal" : Boolean,
    "help": Boolean
};

var shortHands = {
    "p" : ["--password"],
    "t" : ["--tcp-port"],
    "i" : ["--immortal"],
    "h" : ["--help"]
};

var parsed = nopt(knownOpts,shortHands,process.argv,2);

var immortal = parsed['immortal'];
if(immortal == null) immortal = false;
if(immortal == true) {
  stderr.write("immortal mode - exceptions will be ignored (use during critical performances, not during development/practice)\n");
  process.on('uncaughtException', function(err) {
    // do nothing on uncaught exceptions in order to hopefully
    // not disrupt a time-limited performance that is in progress
  });
}

if(parsed['help']!=null) {
    stderr.write("usage:\n");
    stderr.write(" --help (-h)               this help message\n");
    stderr.write(" --password [word] (-p)    password to authenticate OSC messages to server (required)\n");
    stderr.write(" --tcp-port (-t) [number]  TCP port for plain HTTP and WebSocket connections (default: 8002)\n");
    stderr.write(" --immortal (-i)           for use in performance with tested code (exceptions are silently ignored)\n");
    process.exit(1);
}

var password = parsed['password'];
if(password == null) {
    stderr.write("Error: --password option is not optional!\n");
    stderr.write("use --help to display available options\n");
    process.exit(1);
}
if(password == "true") {
  stderr.write("Error: --password option is not optional (nor can it be the text 'true')!\n");
  stderr.write("use --help to display available options\n");
  process.exit(1);
}
if(password[0]=='-') {
  stderr.write("Error: --password cannot begin with a dash (to avoid confusion with other options)");
  stderr.write("use --help to display available options\n");
  process.exit(1);
}


var tempoCps = 2.0; // 120 BPM to start
var tempoAt = (new Date).getTime()/1000;
var tempoBeat = 0.0; // beat 0 at server launch

function logTempo() {
  console.log("tempo at " + tempoAt + " is " + tempoCps + " CPS (beat=" + tempoBeat + ")");
}
logTempo();

var tcpPort = parsed['tcp-port'];
if(tcpPort==null) tcpPort = 8002;

var estuaries = new Array;
var texts = new Array;
var labels = new Array;

// create HTTP (Express) server
var server = http.createServer();
var app = express();
app.use('/',express.static(__dirname +"/Estuary.jsexe",{index: "index.html"}));
server.on('request',app);

// create WebSocket server
var wss = new WebSocket.Server({server: server,perMessageDeflate: false});
wss.broadcast = function(data) {
  try {
    var s = JSON.stringify(data);
    for (let i of wss.clients) {
      try {
        i.send(s);
      }
      catch(e) {
        console.log("warning: exception in websocket broadcast to specific client");
      }
    }
  }
  catch(e) {
    console.log("warning: exception in stringifying JSON data")
  }
};
wss.broadcastNoOrigin = function(originWs,data) {
  try {
    var s = JSON.stringify(data);
    for (let i of wss.clients) {
      try {
	if( i !== originWs) {
          i.send(s);
	}
      }
      catch(e) {
        console.log("warning: exception in websocket broadcast to specific client");
      }
    }
  }
  catch(e) {
    console.log("warning: exception in stringifying JSON data")
  }
};

function sendObject(ws,data) {
  var s;
  try {
    s = JSON.stringify(data);
  }
  catch(e) {
    console.log("warning: exception in sendObject stringifying JSON data");
    return;
  }
  try {
    ws.send(s);
  }
  catch(e) {
    console.log("warning: exception in sendObject during websocket send");
  }
}
  
wss.on('connection',function(ws) {
  var ip = ws.upgradeReq.connection.remoteAddress;
  console.log("new WebSocket connection: " + ip);

  // send stored state of estuary edit panels, text edit panels and labels to new client
  for (var k in estuaries) sendObject(ws,{ 'EEdit':parseInt(k), 'c':estuaries[k], p:'' });
  for (var k in texts) sendObject(ws,{ 'TEdit':parseInt(k), 'c':texts[k], p:'' });
  for (var k in labels) sendObject(ws,{ 'LEdit':parseInt(k), 't':labels[k], p:'' });
  
  ws.on('message',function(m) {
      var n;
      try {
        n = JSON.parse(JSON.parse(m));
      }
      catch(e) {
        console.log("exception in processing incoming message (possibly incorrectly formatted JSON)");
        n = {};
      }
      if(n.p != password) {
        console.log("request with invalid password from " + ip + ", invalid password is " + n.p);
      }
      else if(n.TEdit != null) {
        console.log("TEdit " + n.TEdit + " " + n.c);
        var o = { 'TEdit':n.TEdit, 'c':n.c, p: '' };
	texts[n.TEdit] = n.c;
        wss.broadcastNoOrigin(ws,o);
      }
      else if(n.TEval != null) {
        console.log("TEval " + n.TEval + " " + n.c);
        var o = { 'TEval':n.TEval, 'code':n.c, p: '' };
        wss.broadcastNoOrigin(ws,o);
      }
      else if(n.LEdit != null) {
        console.log("LEdit " + n.LEdit + " " + n.t);
        var o = { 'LEdit':n.LEdit, 't':n.t, p: '' };
	labels[parseInt(n.LEdit)] = n.t;
        wss.broadcastNoOrigin(ws,o);
      }
      else if(n.EEdit != null) {
        console.log("EEdit" + m);
        var o = { 'EEdit':n.EEdit, 'c':n.c, p: '' };
	estuaries[n.EEdit] = n.c;
        wss.broadcastNoOrigin(ws,o);
      }
      else if(n.Chat != null) {
        console.log("Chat from " + n.n + ": " + n.Chat);
        var o = { Chat:n.Chat, n:n.n, p: '' };
        wss.broadcast(o);
      }
      else if(n.Tempo != null) {
        console.log("Error: received Tempo message but tempo can only be changed by TempoChange");
      }
      else if(n.Change != null) {
        console.log("Change " + n.TempoChange);
        // recalculate tempo grid following tempo change
        var now = (new Date).getTime()/1000; console.log("now = " + now); // time in seconds since 1970
	console.log("old tempoAt = " + tempoAt);
        var elapsedTime = now - tempoAt; console.log("elapsedTime = " + elapsedTime);
        var elapsedBeats = elapsedTime * tempoCps; console.log("elapsedBeats = " + elapsedBeats);
        var beatAtNow = tempoBeat + elapsedBeats; console.log("beatAtNow = " + beatAtNow);
        tempoAt = now;
        tempoCps = n.Change;
        tempoBeat = beatAtNow;
        logTempo();
        // broadcast new tempo grid to all clients
        var o = { 'Tempo': tempoCps, 'at': tempoAt, 'beat': tempoBeat, p: '' };
        wss.broadcast(o);
      }
  });

  ws.on('close',function() {
    console.log("connection to " + ip + " closed");
  });
});

// make it go
server.listen(tcpPort, function () { console.log('Listening on ' + server.address().port) });
