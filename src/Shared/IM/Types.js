function id(a) {
        return a;
}

exports.fromJSDate = id;
exports.fromWS = id;
exports.toWS = id;

exports.eqWS = function(w, s) {
        return w == s;
}