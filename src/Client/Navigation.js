import { isWeb } from '../Client.Platform/foreign.js';

export function openExternal(url) {

    return function () {
        if (isWeb)
            window.location.href = url;
        else
            NativeModules.MeroChatLinks.open(url);
    };
}

export function scrollToAnchor(id) {

    return function () {
        if (isWeb)
            document.getElementById(id)?.scrollIntoView();
        else
            lynx.createSelectorQuery().select(`#${id}`).invoke({ method: 'scrollIntoView', params: { scrollIntoViewOptions: { behavior: 'smooth', block: 'start' } } }).exec();
    };
}
