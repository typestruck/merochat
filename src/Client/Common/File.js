let maxWidth = 50 * parseFloat(getComputedStyle(document.querySelector('body')).fontSize),
    quality = 0.7;

export async function resizeAndSendFile_(file, cb) {
    let bitmap = await window.createImageBitmap(file),
        [newWidth, newHeight] = calculateSize(bitmap),
        canvas = document.createElement("canvas");

    canvas.width = newWidth;
    canvas.height = newHeight;

    let ctx = canvas.getContext("2d");

    ctx.drawImage(bitmap, 0, 0, newWidth, newHeight);
    cb(canvas.toDataURL(file.type, quality))();
}

function calculateSize(bitmap) {
    let width = bitmap.width,
        height = bitmap.height;

    if (width > maxWidth) {
        if (width > height)
            height = Math.round((height * maxWidth) / width);

        width = maxWidth;
    }

    return [width, height];
}