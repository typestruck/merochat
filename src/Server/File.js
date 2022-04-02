let b2 = require('b2-js').default;
let b, bucket;

exports.init_ = async function (applicationKeyId, applicationKey) {
    b = await b2.authorize({ applicationKeyId, applicationKey });
    bucket = await b.bucket("ourmelon");
}

exports.upload_ = async function (fileName, buffer) {
    await bucket.upload('upload/' + fileName, buffer);
}