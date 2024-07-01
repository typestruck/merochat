import imageType from 'image-type';

export async function upload_(fileName, buffer) {
}

export function realFileExtension_(buffer) {
    return async function() {
        let r = await imageType(buffer);
        return r.ext || '';
    }
}