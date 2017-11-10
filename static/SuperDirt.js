SuperDirt = function () {
  this.status = "initializing...";
  this.wsReady = false;
  this.setUrl("ws://127.0.0.1:7772");
}

SuperDirt.prototype.setUrl = function(x) {
  if(x == this.url) return;
  if(this.wsReady) {
    this.close();
  }
  this.url = x;
  this.connect();
}

SuperDirt.prototype.log = function(x) {
  console.log("SuperDirt: " + x);
  this.status = x;
}

SuperDirt.prototype.connect = function() {
  this.log("opening connection to " + this.url + "...");
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
      // closure.log("closed (retrying in 5 seconds)");
      closure.wsReady = false;
      closure.ws = null;
      setTimeout(function() {
        closure.connect();
      },5000);
    };
    this.ws.onmessage = function (m) {
      closure.onMessage(m);
    }
  }
  catch(e) {
    this.log("exception in new WebSocket (retry in 5 seconds)");
    setTimeout(function() {
      closure.connect();
    },5000);
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
