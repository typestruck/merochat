export function create(url) {
    return function (protocols) {
        return function () {
            "background only";
            //lynxjs can only load this module in the background thread
            let WebSocket = require("@lynx-js/websocket").default;

            return new WebSocket(url, protocols);
        };
    };
}

export function url(ws) {
    return function () {
        "background only";

        return ws.url;
    };
}

export function readyStateImpl(ws) {
    return function () {
        "background only";

        return ws.readyState;
    };
}

export function bufferedAmount(ws) {
    return function () {
        "background only";

        return ws.bufferedAmount;
    };
}

export function extensions(ws) {
    return function () {
        "background only";

        return ws.extensions;
    };
}

export function protocol(ws) {
    return function () {
        "background only";

        return ws.protocol;
    };
}

export function close(ws) {
    return function () {
        "background only";

        return ws.close();
    };
}

export function getBinaryTypeImpl(ws) {
    return function () {
        "background only";

        return ws.binaryType;
    };
}

export function setBinaryTypeImpl(ws) {
    return function (bt) {
        return function () {
            "background only";
            ws.binaryType = bt;
        };
    };
}

export function sendImpl(ws) {
    return function (value) {
        return function () {
            "background only";
            ws.send(value);
        };
    };
}
