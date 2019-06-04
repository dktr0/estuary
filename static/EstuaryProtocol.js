EstuaryProtocol = function () {
  this.status = "initializing...";
  this.wsReady = false;
  this.readyStatChangeListeners = [];
  this.responses = new Array;

  var port = location.port !== '' ? ':' + location.port : '';
  this.setUrl("wss://" + location.hostname + port + location.pathname);
}

EstuaryProtocol.prototype.setUrl = function(x) {
  if(x == this.url) return;
  if(this.wsReady) {
    this.close();
  }
  this.url = x;
  this.connect();
}

EstuaryProtocol.prototype.onReadyStateChange = function (cb) {
  this.readyStatChangeListeners.push(cb);
}

EstuaryProtocol.prototype.log = function(x) {
  console.log("EstuaryProtocol (" + this.url + "): " + x);
  this.status = x;
}

EstuaryProtocol.prototype.changeStatus = function (status, wsReady) {
  this.log(status);
  this.wsReady = wsReady;
  this.readyStatChangeListeners.forEach(cb => cb(wsReady));
}

EstuaryProtocol.prototype.connect = function () {
  this.log("opening connection");
  window.WebSocket = window.WebSocket || window.MozWebSocket;
  var closure = this;
  try {
    this.ws = new WebSocket(this.url);
    this.ws.onopen = this.changeStatus.bind(this, 'connection open', true);
    this.ws.onerror = this.changeStatus.bind(this, 'error', false);
    this.ws.onclose = function () {
      closure.changeStatus('closed (retry in 1s)', false);
      closure.ws = null;
      setTimeout(function() {
        closure.connect();
      }, 1000);
    }
    this.ws.onmessage = function (m) {
      closure.onMessage(m);
    }
  }
  catch(e) {
    this.log("exception (retry in 1s)");
    setTimeout(function() {
      closure.connect();
    }, 1000);
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
