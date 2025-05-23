export let scrollEventName = (typeof window !== "undefined" && !("onscrollend" in window)) ? "scroll" : "scrollend";
