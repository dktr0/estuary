PeerProtocol = function () {
  this.status = 0;
  const script = document.createElement('script');
  script.setAttribute('type', 'text/javascript');
  script.setAttribute('src', 'https://cdn.jsdelivr.net/npm/peerjs@0.3.20/dist/peer.min.js');
  document.getElementsByTagName('head')[0].appendChild(script);
  script.onerror = () => { throw new Error('Could not load script.'); };
  var closure = this;
  script.onload = () => {
    closure.status = 1;
  };
}

PeerProtocol.prototype.startStreaming = function() {
  if(this.status != 1) return;
  this.peer = new Peer({
    debug: true,
    host: 'localhost',
    secure: false,
    port: 9000,
    path: '/peers'
  });
  var closure = this;
  this.peer.on('open', function(id) {
    console.log(`PeerProtocol OPEN: id=${id}`);
    closure.id = id;
    closure.status = 2;
  });
  this.peer.on('error', function(err) {
    console.log(`PeerProtocol ERROR: ${err}`);
    closure.status = 1;
  });
  this.peer.on('disconnected', function() {
    console.log('PeerProtocol disconnected...');
    closure.status = 1;
  });
  this.peer.on('connection', function(conn) {
    closure.status = 3;
    conn.on('data', function(data) {
      if (data === 'CALL') {
        const streamNode = ___setEstuaryAudioDestination('stream');
        closure.peer.call(conn.peer, streamNode.stream);
      }
    });
  });
}
