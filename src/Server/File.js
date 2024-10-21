import imageType from 'image-type';

//webm for audio files
function webm(buffer) {
    return buffer[0] === 26 &&
        buffer[1] === 69 &&
        buffer[2] === 223 &&
        buffer[3] === 163 ? '.webm' : '';
}

export function realFileExtension_(buffer) {
    return async function() {
        let r = await imageType(buffer);
        return r.ext || webm(buffer) || '';
    }
}