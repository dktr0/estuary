EstuaryProtocol = function () {
  this.wsReady = false;
}

EstuaryProtocol.prototype.setUrl = function(x) {
  if(x == this.url) return;
  if(this.wsReady) {
    this.close();
  }
  this.url = x;
  this.connect();
}

EstuaryProtocol.prototype.connect = function() {
  console.log("EstuaryProtocol: opening websocket connection to " + this.url + "...");
  window.WebSocket = window.WebSocket || window.MozWebSocket;
  var closure = this;
  try {
    this.ws = new WebSocket(this.url);
    this.ws.onopen = function () {
      console.log("EstuaryProtocol: websocket connection opened");
      closure.wsReady = true;
    };
    this.ws.onerror = function () {
      console.log("EstuaryProtocol: websocket error");
      closure.wsReady = false;
    };
    this.ws.onclose = function () {
      console.log("EstuaryProtocol: websocket closed (retrying in 1 second)");
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
    console.log("disregarding exception in new WebSocket (retry in 1 second)");
    setTimeout(function() {
      closure.connect();
    },1000);
  }
}

EstuaryProtocol.prototype.onMessage = function(m) {
  console.log("EstuaryProtocol onMessage:" + m);
}

EstuaryProtocol.prototype.send = function(o) {
  if(!this.wsReady)return;
  try {
    this.ws.send(JSON.stringify(o));
  }
  catch(e) {
    console.log("EstuaryProtocol: warning - exception in websocket send");
  }
}
