process.title = 'estuaryHttp';
console.log("estuaryHttp");

// dependencies
var http = require('http');
var express = require('express');
var nopt = require('nopt');

// parse command-line options
var knownOpts = { "port" : [Number, null], "help": Boolean };
var shortHands = { "p" : ["--port"], "h" : ["--help"] };
var parsed = nopt(knownOpts,shortHands,process.argv,2);

if(parsed['help']!=null) {
  console.log("Help:");
  console.log(" This is a simple HTTP server meant to accompany the Estuary WebSocket server.")
  console.log(" It listens for incoming connections and serves files...");
  console.log("\nOptions:");
  console.log(" --help (-h)           this help message");
  console.log(" --port (-p) [number]  TCP port for plain HTTP and WebSocket connections (default: 9161)\n");
  process.exit(1);
}

var port = parsed['port'];
if(port==null) port = 9161;

var server = http.createServer();
var app = express();
app.use('/',express.static(__dirname +"/staticWebContent",{index: "default.html"}));
server.on('request',app);
server.listen(port, function () { console.log('Listening on ' + server.address().port) });
