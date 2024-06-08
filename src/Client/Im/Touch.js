let runningTouchStartX = 0,
    runningTouchStartY = 0;

export function touchStart(event) {
    runningTouchStartX = event.changedTouches[0].screenX;
    runningTouchStartY = event.changedTouches[0].screenY;
}

export function touchEnd(event) {
    let touchStartX = runningTouchStartX,
        touchEndX = event.changedTouches[0].screenX,
        touchStartY = runningTouchStartY,
        touchEndY = event.changedTouches[0].screenY;

    runningTouchStartX = 0;
    runningTouchStartY = 0;

    return [touchStartX, touchEndX, touchStartY, touchEndY];
}