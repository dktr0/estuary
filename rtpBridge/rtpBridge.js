const express = require('express');
const ExpressPeerServer = require('peer').ExpressPeerServer;
const app = express();
app.get('/', function (req, res, next) {
    res.send('Use the peers endpoint ');
});
const server = app.listen(9000);
const peerserver = ExpressPeerServer(server, {
    debug: true,
    key: 'peerjs' // Cant change...
});
app.use('/peers', peerserver);
