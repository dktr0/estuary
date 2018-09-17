EstuaryProtocol = function () {
  this.status = "initializing...";
  this.wsReady = false;
  this.responses = new Array;
  var port = "";
  if(location.port != "") port = ":" + location.port;
  this.setUrl("ws://" + location.hostname + port + location.pathname);
}

EstuaryProtocol.prototype.setUrl = function(x) {
  if(x == this.url) return;
  if(this.wsReady) {
    this.close();
  }
  this.url = x;
  this.connect();
}

EstuaryProtocol.prototype.log = function(x) {
  console.log("EstuaryProtocol (" + this.url + "): " + x);
  this.status = x;
}

EstuaryProtocol.prototype.connect = function() {
  this.log("opening connection");
  window.WebSocket = window.WebSocket || window.MozWebSocket;
  var closure = this;
  try {
    this.ws = new WebSocket(this.url);
    this.ws.onopen = function () {
      closure.log("connection open");
      closure.wsReady = true;
    };
    this.ws.onerror = function () {
      closure.log("error");
      closure.wsReady = false;
    };
    this.ws.onclose = function () {
      closure.log("closed (retry in 1s)");
      closure.wsReady = false;
      closure.ws = null;
      setTimeout(function() {
        closure.connect();
      },1000);
    };
    this.ws.onmessage = function (m) {
      closure.onMessage(m);
    }
  }
  catch(e) {
    this.log("exception (retry in 1s)");
    setTimeout(function() {
      closure.connect();
    },1000);
  }
}

EstuaryProtocol.prototype.onMessage = function(m) {
   try {
     var n = JSON.parse(m.data);
   }
   catch(e) {
     this.log("parsing exception");
     console.log("dump: " + JSON.stringify(m.data));
     return;
   }
   this.responses.push(n);
}

EstuaryProtocol.prototype.send = function(o) {
  if(!this.wsReady)return;
  try {
    this.ws.send(JSON.stringify(o));
  }
  catch(e) {
    this.log("send exception");
  }
}

EstuaryProtocol.prototype.getResponses = function() {
  var x = this.responses;
  this.responses = new Array;
  return JSON.stringify(x);
}
