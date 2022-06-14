import b2 from 'b2-js';
let b, bucket;

export async function init_(applicationKeyId, applicationKey) {
    b = await b2.authorize({ applicationKeyId, applicationKey });
    bucket = await b.bucket("ourmelon");
}

export async function upload_(fileName, buffer) {
    await bucket.upload('upload/' + fileName, buffer);
}