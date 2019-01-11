SuperDirt = function () {
  this.url = "ws://127.0.0.1:7772";
  this.status = "initializing...";
  this.wsReady = false;
  this.active = false;
  this.connectionInProgress = false;
}

SuperDirt.prototype.setActive = function(x) {
  if(x == this.active) return;
  this.active = x;
  if(x == true) this.connect();
}

SuperDirt.prototype.log = function(x) {
  console.log("SuperDirt: " + x);
  this.status = x;
}

SuperDirt.prototype.connect = function() {
  if(this.active == false) return;
  if(this.wsReady == true) return;
  if(this.connectionInProgress == true) return;
  connectionInProgress = true;

  this.log("opening SuperDirt socket connection to " + this.url + "...");
  window.WebSocket = window.WebSocket || window.MozWebSocket;
  var closure = this;
  try {
    this.ws = new WebSocket(this.url);
    this.ws.onopen = function () {
      closure.log("connection open");
      closure.wsReady = true;
      closure.connectionInProgress = false;
    };
    this.ws.onerror = function () {
      closure.log("error");
      closure.wsReady = false;
      closure.connectionInProgress = false;
    };
    this.ws.onclose = function () {
      closure.log("SuperDirt socket closed (retrying in 5 seconds)");
      closure.connectionInProgress = false;
      closure.wsReady = false;
      closure.ws = null;
      if(closure.active == true) {
        setTimeout(function() {
          closure.connect();
        },5000);
      }
    };
    this.ws.onmessage = function (m) {
      closure.onMessage(m);
    }
  }
  catch(e) {
    closure.log("exception in new WebSocket (retry in 5 seconds)");
    closure.connectionInProgress = false;
    closure.wsReady = false;
    if(closure.active == true) {
      setTimeout(function() {
        closure.connect();
      },5000);
    }
  }
}

SuperDirt.prototype.playSample = function(o) {
  if(!this.wsReady)return;
  try {
    this.ws.send(JSON.stringify(o));
  }
  catch(e) {
    this.log("warning - exception in websocket send");
  }
}
