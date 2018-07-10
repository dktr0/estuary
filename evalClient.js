var nopt = require('nopt');
var WebSocket = require('ws');

// parse command-line options
var knownOpts = {
    "server" : [String, null],
    "port" : [String, null],
    "help": Boolean,
    "newlines-as-spaces" : Boolean
};

var shortHands = {
    "s" : ["--server"],
    "p" : ["--port"],
    "n" : ["--newlines-as-spaces"],
    "h" : ["--help"],
};

var parsed = nopt(knownOpts,shortHands,process.argv,2);

if(parsed['help']!=null) {
    console.log("evalClient for estuary");
    console.log(" --help (-h)                 this help message");
    console.log(" --server (-s) [address]     address of server's downstream (default:localhost)");
    console.log(" --port (-p) [number]        TCP port for WebSocket communication with server");
    console.log(" --newlines-as-spaces (-n)   converts any received newlines to spaces on stdout");
    process.exit(1);
}

// before setting up connections, set some defaults and look for problems
var newlines = parsed['newlines-as-spaces'];
var server = parsed['server'];
if(server == null) { server = "localhost"; }
var port = parsed['port'];
if(port == null) { port = 80; }
var wsAddress = "ws://" + server + ":" + (port.toString());

var connectWs = function() {
    console.log("\"estuary evalClient: connecting to " + wsAddress + "...\".postln");
    var ws = new WebSocket(wsAddress);
    var udp,oscFunction,feedbackFunction;

    ws.on('open',function() {
      console.log("\"estuary evalClient: connected to " + wsAddress + "\".postln");
    });

    ws.on('message',function(m) {
      var n = JSON.parse(m);
      if(n.TEval != null) {
        var s = n.c;
        if(newlines) { // convert newline sequences to whitespace
            s = s.replace(/\n|\n\r|\r\n|\r/g," ");
        }
	console.log(s);
      }
    });

    ws.on('close',function() {
      console.log("\"estuary evalClient: websocket connection " + wsAddress + " closed\".postln");
      setTimeout(connectWs,5000);
    });

};

if(wsAddress != null) connectWs();

process.on('SIGINT', function() {
  ws.close();
});
