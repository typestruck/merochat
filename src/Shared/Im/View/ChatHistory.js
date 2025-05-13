export let scrollEventName = (typeof window !== "undefined" && !("onscrollend" in window)) ? "scroll" : "scrollend";

let userHasScrolled = false;

export function runScrollEvent_(event, noMessage, scrollDownMessage, doNotScrollDownMessage, fetchHistoryMessage) {

    let message = noMessage;

    if (event.target.scrollTop >= event.target.scrollHeight - event.target.offsetHeight) {
        if (userHasScrolled) {
            //scrolled to the bottom of the page, so reset scroll down toggle
            message = scrollDownMessage;
            userHasScrolled = false;
        }
    } else {
        userHasScrolled = true;

        if (event.target.scrollTop < 1.0) {
            //scrolled to the top of the page
            message = fetchHistoryMessage;
        } else {
            //user scrolled the chat history so do not scroll down to last incoming chat message
            message = doNotScrollDownMessage;
        }
    }

    console.log(event.target.scrollTop, event.target.scrollHeight - event.target.offsetHeight, message, userHasScrolled);

    return message;
}