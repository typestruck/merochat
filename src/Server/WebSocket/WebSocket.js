import WebSocket from 'ws';

/* Server methods */
export function createWebSocketServer_(options, callback) {
      return new WebSocket.Server(options, callback);
}

export function onConnection_(wss, handleConnection) {
      wss.on('connection', handleConnection);
}

export function onServerError_(wss, handleError) {
      wss.on('error', handleError);
}

/* WebSocket methods */
export function onMessage_(ws, handleMessage) {
      ws.on('message', handleMessage);
}

export function onClose_(ws, handleClose) {
      ws.on('close', handleClose);
}

export function onServerClose_(ws, handleClose) {
      ws.on('close', handleClose);
}

export function onError_(ws, handleError) {
      ws.on('error', handleError);
}

export function lastPing(lp) {
      return function (connection) {
            connection.lastPingTag = lp;

            return connection;
      }
}

export function getLastPing(connection) {
      return connection.lastPingTag;
}

export function onPong_(ws, handlePong) {
      ws.on('pong', handlePong);
}

export function ping_(ws, pinger) {
      ws.ping(pinger);
}

export function sendMessage_(ws, message) {
      ws.send(message);
}

export function close_(ws, code, reason) {
      ws.close(code, reason);
}

export function terminate_(ws) {
      ws.terminate();
}

export function createWebSocket_(url, cookie) {
      return new WebSocket(url, { headers: { 'Cookie': `merochat=${cookie}`}});
}

export function onOpen_(ws, handleOpen) {
      ws.on('open', handleOpen);
}
