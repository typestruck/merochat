let maxWidth = typeof window !== "undefined" && window.matchMedia('(max-width:1279px)').matches ?
    parseInt(getComputedStyle(document.querySelector('#im')).width) - 20 :
    50 * 16, // max width of messages times font size
    quality = 0.94;

export async function resizeAndSendFile_(file, cb) {
    let bitmap = await window.createImageBitmap(file),
        [newWidth, newHeight] = calculateSize(bitmap),
        canvas = document.createElement("canvas");

    canvas.width = newWidth;
    canvas.height = newHeight;

    let ctx = canvas.getContext("2d");

    ctx.drawImage(bitmap, 0, 0, newWidth, newHeight);
    cb(newWidth)(newHeight)(canvas.toDataURL(file.type, quality))();

}

function calculateSize(bitmap) {
    let width = bitmap.width,
        height = bitmap.height;

    if (width > maxWidth) {
        height = Math.round((height * maxWidth) / width);
        width = maxWidth;
    }

    return [width, height];
}

export function fileSize(base64) {
    let str = base64.substr(base64.indexOf(',') + 1);

    return ('fromBase64' in Uint8Array ? Uint8Array.fromBase64(str) : new Uint8Array(Buffer.from(str, 'base64'))).length;
}