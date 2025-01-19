import imageType from 'image-type';

//webm and aac for audio files
function webm(buffer) {
    return buffer[0] === 26 &&
        buffer[1] === 69 &&
        buffer[2] === 223 &&
        buffer[3] === 163 ? 'webm' : '';
}

function aac(buffer) {
    console.log(buffer[0], buffer[1]);
    return buffer[0] === 255 && (buffer[1] === 241 || buffer[1] === 249) ? 'aac' : '';
}

export function realFileExtension_(buffer) {
    return async function () {
        let r = await imageType(buffer);
        return r.ext || webm(buffer) || aac(buffer) || '';
    }
}