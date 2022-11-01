let constant = 20,
    initialHeight;

export function resizeTextarea_(textarea) {
    if (initialHeight === undefined)
        initialHeight = textarea.clientHeight - constant;

    if (textarea.value === '')
        textarea.style.height = initialHeight + "px";

    textarea.style.height = (textarea.scrollHeight - constant) + "px";
}
