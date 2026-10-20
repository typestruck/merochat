let key = 'merochat-theme',
    light = 'light',
    dark = 'dark',
    //lynx does not have these
    hasDocument = typeof document !== 'undefined',
    hasLocalStorage = typeof localStorage !== 'undefined';

globalThis.switchTheme = (theme, fromEvent = false) => {
    let properties = {
        '--background-color': theme === light ? '#FBFEFB' : '#1D2B35',
        '--text-color': theme === light ? '#1B1F3B' : '#FBFEFB',
        '--external-accent': theme === light ? '#EFE5DC' : '#274958',
        '--other-heading-color': theme === light ? '#42858C' : '#64FCD9',
        '--light-link-color': theme === light ? '#FBFEFB' : '#327B4F',
        '--im-background-color': theme === light ? '#F0E8DD' : '#393E41',
        '--im-second-background-color': theme === light ? '#B8574F' : '#26292C',
        '--im-chat-background-image': theme === light ? 'url(/file/default/chat-background-light.svg)' : 'url(/file/default/chat-background-dark.svg)',
    };

    if (hasDocument) {
        for (let [name, value] of Object.entries(properties))
            document.documentElement.style.setProperty(name, value);
    }
    else
        globalThis.themeProperties = properties;

    if (fromEvent && hasLocalStorage)
        localStorage.setItem(key, theme);
}

let theme = dark;

if (hasLocalStorage && localStorage.getItem(key))
    theme = localStorage.getItem(key);
else if (typeof window !== 'undefined' && window.matchMedia && window.matchMedia('(prefers-color-scheme: light)').matches)
    theme = light;

globalThis.switchTheme(theme);

if (hasDocument)
    document.addEventListener('DOMContentLoaded', () => {
        document.querySelector('#light-theme-switch')?.addEventListener('click', () => globalThis.switchTheme(light, true));
        document.querySelector('#dark-theme-switch')?.addEventListener('click', () => globalThis.switchTheme(dark, true));
    }, { once: true });
