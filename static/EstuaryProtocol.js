EstuaryProtocol = function () {
  this.status = "initializing...";
  this.wsReady = false;
  this.edits = new Array;
  this.setUrl("ws://" + location.hostname + ":" + location.port);
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
  console.log("EstuaryProtocol: " + x);
  this.status = x;
}

EstuaryProtocol.prototype.connect = function() {
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
      closure.log("closed (retrying in 1 second)");
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
    this.log("exception in new WebSocket (retry in 1 second)");
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
     this.log("parsing exception in onMessage");
     console.log("dump: " + JSON.stringify(m.data));
     return;
   }
   try {
     if(n.TEdit != null || n.TEval != null || n.LEdit != null | n.EEdit != null | n.Chat != null) {
       this.edits.push(n);
     }
     else if(n.Tempo != null) {
       // console.log("EstuaryProtocol onMessage Tempo: " + n.Tempo + " CPS at " + n.at + " (beat=" + n.beat + ")");
       this.tempoCps = n.Tempo;
       this.tempoAt = n.at;
       this.tempoBeat = n.beat;
       this.edits.push(n);
     }
     else if(n.Change != null) {
       // console.log("EstuaryProtocol onMessage TempoChange (ignoring)");
     }
     else {
       this.log("unrecognized message in onMessage");
     }
   }
   catch(e) {
     this.log("exception in onMessage");
   }
}

EstuaryProtocol.prototype.send = function(o) {
  if(!this.wsReady)return;
  try {
    this.ws.send(JSON.stringify(o));
  }
  catch(e) {
    this.log("warning - exception in websocket send");
  }
}

EstuaryProtocol.prototype.getEdits = function() {
  var x = this.edits;
  this.edits = new Array;
  return JSON.stringify(x);
}
