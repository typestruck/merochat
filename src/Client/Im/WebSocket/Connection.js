import WebSocket from "@lynx-js/websocket";

export function create(url) {
  return function (protocols) {
    return function () {
      return new WebSocket(url, protocols);
    };
  };
}

export function url(ws) {
  return function () {
    return ws.url;
  };
}

export function readyStateImpl(ws) {
  return function () {
    return ws.readyState();
  };
}

export function bufferedAmount(ws) {
  return function () {
    return ws.bufferedAmount;
  };
}

export function extensions(ws) {
  return function () {
    return ws.extensions;
  };
}

export function protocol(ws) {
  return function () {
    return ws.protocol;
  };
}

export function close(ws) {
  return function () {
    return ws.close();
  };
}

export function getBinaryTypeImpl(ws) {
  return function () {
    return ws.binaryType;
  };
}

export function setBinaryTypeImpl(ws) {
  return function (bt) {
    return function () {
      ws.binaryType = bt;
    };
  };
}

export function sendImpl(ws) {
  return function (value) {
    return function () {
      ws.send(value);
    };
  };
}
