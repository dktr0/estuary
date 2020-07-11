var estuaryProtocolInspector = (function () {
  function onRecvMsg(protocol, callback) {
    var oldOnMessage = protocol.onMessage;
    protocol.onMessage = function (/*...arguments*/) {
      var oldLen = protocol.responses.length;

      var result = oldOnMessage.apply(protocol, arguments);

      var newLen = protocol.responses.length;
      if (oldLen !== newLen) {
        // The parse succeeded and a response was added to
        // the end of the responses array.
        var response = protocol.responses[newLen - 1];

        // Call the callback with the serialized response
        callback(JSON.stringify(response));
      } else {
        // Parse failed, invoke with null to signal bad message received
        callback(null);
      }

      return result;
    };
  }

  function onSendMsg(protocol, callback) {
    var oldSend = protocol.send;
    protocol.send = function (/*...arguments*/) {
      var data = arguments[0];
      try {
        JSON.stringify(data);
        callback(data);
      } catch (e) {
        callback(null);
      }
      return oldSend.apply(protocol, arguments);
    };
  }

  function onConnect(protocol, callback) {
    if (protocol.wsReady)
      return callback();

    protocol.onReadyStateChange(function (wsReady) {
      if (wsReady)
        callback();
    });
  }

  return {
    onRecvMsg: onRecvMsg,
    onSendMsg: onSendMsg,
    onConnect: onConnect
  }
})();