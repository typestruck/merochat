import imageType from 'image-type';
import { NsfwSpy } from '@nsfwspy/node';

const nsfw = new NsfwSpy("file://./file/nsfw/model.json");
await nsfw.load();

//webm and mp4 for audio files
function webm(buffer) {
    return buffer[0] === 26 &&
        buffer[1] === 69 &&
        buffer[2] === 223 &&
        buffer[3] === 163 ? 'webm' : '';
}

//apple (and chrome?) shit
function mp4(buffer) {
    return buffer[0] === 0 &&
        buffer[1] === 0 &&
        buffer[2] === 0 &&
        (buffer[3] === 32 || buffer[3] === 36) &&
        buffer[4] === 102 &&
        buffer[5] === 116 &&
        buffer[6] === 121 &&
        buffer[7] === 112 &&
        (buffer[8] === 105 || buffer[8] === 77) ? 'mp4' : '';
}

export function realFileExtension_(buffer) {
    return async function () {
        let r = await imageType(buffer);
        return r.ext || webm(buffer) || mp4(buffer) || '';
    }
}

export function isNsfw_(filePath) {
    return async function () {
        let prediction = await nsfw.classifyImageFile(filePath);

        return prediction.isNsfw;
    }
}