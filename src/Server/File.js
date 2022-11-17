import b2 from 'b2-js';
import imageType from 'image-type';

let b, bucket;

export async function init_(applicationKeyId, applicationKey) {
    b = await b2.default.authorize({ applicationKeyId, applicationKey });
    bucket = await b.bucket('ourmelon');
}

export async function upload_(fileName, buffer) {
    await bucket.upload('upload/' + fileName, buffer);
}

export function realFileExtension_(buffer) {
    return async function() {
        let r = await imageType(buffer);
        return r.ext || '';
    }
}