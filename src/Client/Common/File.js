let maxWidth = typeof window !== "undefined" && window.matchMedia('(max-width:1279px)').matches ?
    parseFloat(getComputedStyle(document.querySelector('#contact-list')).width) - 20 :
    50 * 16,
    quality = 0.9;

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