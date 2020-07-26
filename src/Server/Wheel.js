const zmq = require("zeromq");
const sock = new zmq.Push;

sock.bind("tcp://127.0.0.1:3000");
console.log("Wheel push now up on tcp://localhost:3000");

exports.sendWheelMessage_ = function sendWheelMessage_(message){
        sock.send(message);
}
