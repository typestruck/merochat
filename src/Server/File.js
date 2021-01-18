let crypto = require('crypto');

exports.sha1 = function (av) {
    return crypto.createHash('sha1').update(av).digest('hex');
}
