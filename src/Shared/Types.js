function id(a) {
        return a;
}

exports.fromInt53 = id;
exports.toInt53 = id;
exports.fromWS = id;
exports.toWS = id;

exports.eqWS = function(w, s) {
        return w == s;
}