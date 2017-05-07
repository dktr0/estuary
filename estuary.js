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

var tcpPort = parsed['tcp-port'];
if(tcpPort==null) tcpPort = 8002;

// create HTTP (Express) server
var server = http.createServer();
var app = express();
app.use('/',express.static(__dirname +"/Estuary.jsexe",{index: "index.html"}));
server.on('request',app);

// create WebSocket server
var wss = new WebSocket.Server({server: server});
wss.broadcast = function(data) {
  for (var i in this.clients) {
    try {
      this.clients[i].send(data);
    }
    catch(e) {
      console.log("warning: exception in websocket broadcast");
    }
  }
};

wss.on('connection',function(ws) {
  // var location = url.parse(ws.upgradeReq.url, true);
  var ip = ws.upgradeReq.connection.remoteAddress;
  console.log("new WebSocket connection: " + ip);

  ws.on('message',function(m) {
      try {
        var n = JSON.parse(m);
        console.log(n);
      }
      catch(e) {
        console.log("exception in processing incoming message (possibly incorrectly formatted JSON)");
        n = {};
      }
      if(n.password != password) {
        console.log("request with invalid password from " + ip);
      }
      else if(n.TextEdit != null) {
        console.log("TextEdit");
        var o = { 'TextEdit':n.TextEdit, 'code':n.code, password: '' };
        try { wss.broadcast(JSON.stringify(o)); }
        catch(e) { console.log("warning: exception in WebSocket send\n"); }
      }
      else if(n.TextEval != null) {
        console.log("TextEval");
        var o = { 'TextEval':n.TextEval, 'code':n.code, password: '' };
        try { wss.broadcast(JSON.stringify(o)); }
        catch(e) { console.log("warning: exception in WebSocket send\n"); }
      }
      else if(n.request == "EstuaryEdit") {
        console.log("EstuaryEdit");
        var o = { 'EstuaryEdit':n.EstuaryEdit, 'code':n.code, password: '' };
        try { wss.broadcast(JSON.stringify(o)); }
        catch(e) { console.log("warning: exception in WebSocket send\n"); }
      }
  });

  ws.on('close',function() {
    console.log("connection to " + ip + " closed");
  });
});

// make it go
server.listen(tcpPort, function () { console.log('Listening on ' + server.address().port) });
