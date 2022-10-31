export function resizeTextarea_(textarea) {
    textarea.style.height = 0;
    textarea.style.height =  (textarea.scrollHeight - 20) + "px";
}
