import imageType from 'image-type';

//webm and mp4 for audio files
function webm(buffer) {
    return buffer[0] === 26 &&
        buffer[1] === 69 &&
        buffer[2] === 223 &&
        buffer[3] === 163 ? 'webm' : '';
}

//apple shit
function mp4(buffer) {
    console.log(buffer[0], buffer[1], buffer[2], buffer[3], buffer[4], buffer[5], buffer[6], buffer[7], buffer[8]);
    return buffer[0] === 102 &&
        buffer[1] === 116 &&
        buffer[2] === 121 &&
        buffer[3] === 112 &&
        buffer[4] === 112 &&
        (buffer[5] === 105 || buffer[5] === 77) &&
        (buffer[6] === 115 || buffer[6] === 83) &&
        (buffer[7] === 111 || buffer[7] === 78) &&
        (buffer[8] === 109 || buffer[8] === 86) ? 'mp4' : '';
}

export function realFileExtension_(buffer) {
    return async function () {
        let r = await imageType(buffer);
        return r.ext || webm(buffer) || mp4(buffer) || '';
    }
}