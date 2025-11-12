let constant = 22,
    initialHeight,
    oldScrollHeight;

export function resizeInput(textarea) {
    if (initialHeight === undefined)
        initialHeight = textarea.clientHeight - constant;

    if (textarea.value === '')
        textarea.style.height = initialHeight + "px";
    else if (textarea.scrollHeight != oldScrollHeight) {
        textarea.style.height = (textarea.scrollHeight - constant) + "px";
    }

    oldScrollHeight = textarea.scrollHeight;
}

