export function handleEvent(root) {
    return function (event) {
        return function () {
            globalThis.themeEvent(root, event);
        };
    };
}

export function onThemeRequested(handler) {
    return function () {
        globalThis.loadTheme = handler;
    };
}

export function onThemeChanged(handler) {
    return function () {
        globalThis.persistTheme = handler;
    };
}

export function systemTheme() {
    'background only';

    return NativeModules.MeroChatTheme.systemTheme();
}
