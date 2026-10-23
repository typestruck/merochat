//likely we can move this into shared/Theme.purs
let key = 'merochat-theme',
    light = 'light',
    dark = 'dark',
    isWeb = typeof document !== 'undefined';

function switchTheme(selectedTheme, root, fromEvent = true) {
    let properties = {
        '--background-color': selectedTheme === light ? '#FBFEFB' : '#1D2B35',
        '--text-color': selectedTheme === light ? '#1B1F3B' : '#FBFEFB',
        '--external-accent': selectedTheme === light ? '#EFE5DC' : '#274958',
        '--other-heading-color': selectedTheme === light ? '#42858C' : '#64FCD9',
        '--light-link-color': selectedTheme === light ? '#FBFEFB' : '#327B4F',
        '--im-background-color': selectedTheme === light ? '#F0E8DD' : '#393E41',
        '--im-second-background-color': selectedTheme === light ? '#B8574F' : '#26292C',
        '--im-chat-background-image': selectedTheme === light ? 'url(/file/default/chat-background-light.svg)' : 'url(/file/default/chat-background-dark.svg)',
    };

    if (isWeb) {
        for (let [name, value] of Object.entries(properties))
            root.style.setProperty(name, value);
    }
    else if (root) {
        lynx.getElementById(root).setProperty(properties);
    }

    if (fromEvent) {
        if (isWeb)
            localStorage.setItem(key, selectedTheme);
        else
            globalThis.persistTheme(selectedTheme)();
    }
}

function themeEvent(root, event) {
    let target = event.currentTarget;

    if (isWeb)
        root = document.documentElement;
    if (event.type === 'layoutchange')
        globalThis.loadTheme(selectedTheme => () => switchTheme(selectedTheme, root, false))();
    else if (target.id === 'light-theme-switch')
        switchTheme(light, root);
    else if (target.id === 'dark-theme-switch')
        switchTheme(dark, root);
}

globalThis.switchTheme = switchTheme;
globalThis.themeEvent = themeEvent;

if (isWeb) {
    switchTheme(localStorage.getItem(key) || (window.matchMedia?.('(prefers-color-scheme: light)').matches ? light : dark), document.documentElement, false);
    document.addEventListener('DOMContentLoaded', () => {
        document.querySelector('#light-theme-switch')?.addEventListener('click', () => switchTheme(light, document.documentElement));
        document.querySelector('#dark-theme-switch')?.addEventListener('click', () => switchTheme(dark, document.documentElement));
    }, { once: true });
}
