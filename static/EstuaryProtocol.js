EstuaryProtocol = function () {
  this.wsReady = false;
  this.edits = new Array;
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
   try {
     var n = JSON.parse(m.data);
   }
   catch(e) {
     console.log("parsing exception in EstuaryProtocol.onMessage");
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
       console.log("warning: unrecognized message in EstuaryProtocol onMessage");
     }
   }
   catch(e) {
     console.log("exception in EstuaryProtocol onMessage");
   }
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

EstuaryProtocol.prototype.getEdits = function() {
  var x = this.edits;
  this.edits = new Array;
  return JSON.stringify(x);
}

