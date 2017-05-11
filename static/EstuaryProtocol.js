EstuaryProtocol = function () {
  this.wsReady = false;
  this.textEdits = new Array;
  this.estuaryEdits = new Array;
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
   if(n.TextEdit != null) {
     console.log("EstuaryProtocol onMessage TextEdit" + parseInt(n.TextEdit));
     this.textEdits[parseInt(n.TextEdit)] = n.code;
     this.edits.push(n);
   }
   else if(n.TextEval != null) {
     // console.log("EstuaryProtocol onMessage TextEval" + n.TextEval);
     // no need to log textevals in the browser for now
   }
   else if(n.EstuaryEdit != null) {
     console.log("EstuaryProtocol onMessage EstuaryEdit" + parseInt(n.EstuaryEdit));
     this.estuaryEdits[parseInt(n.EstuaryEdit)] = n.code;
     this.edits.push(n);
   }
   else if(n.Tempo != null) {
     console.log("EstuaryProtocol onMessage Tempo: " + n.Tempo + " CPS at " + n.at + " (beat=" + b.beat + ")");
     this.tempoCps = n.Tempo;
     this.tempoAt = n.at;
     this.tempoBeat = n.beat;
   }
   else if(n.TempoChange != null) {
     console.log("EstuaryProtocol onMessage TempoChange (ignoring)");
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
  if(o.TextEdit != null) {
    this.textEdits[parseInt(o.TextEdit)] = o.code;
  }
  if(o.EstuaryEdit != null) {
    this.estuaryEdits[parseInt(o.EstuaryEdit)] = o.code;
  }
  if(!this.wsReady)return;
  try {
    this.ws.send(JSON.stringify(o));
  }
  catch(e) {
    console.log("EstuaryProtocol: warning - exception in websocket send");
  }
}

EstuaryProtocol.prototype.getTextEdit = function(n) {
  if(n < this.textEdits.length) {
    return this.textEdits[n];
  }
  else return null;
}


EstuaryProtocol.prototype.getEstuaryEdit = function(n) {
  if(n < this.estuaryEdits.length) {
    return this.estuaryEdits[n];
  }
  else return null;
}

EstuaryProtocol.prototype.getEdits = function() {
  var x = this.edits;
  this.edits = new Array;
  return JSON.stringify(x);
}
