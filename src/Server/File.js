import imageType from 'image-type';

export function realFileExtension_(buffer) {
    return async function() {
        let r = await imageType(buffer);
        return r.ext || '';
    }
}