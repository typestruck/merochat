let key = 'merochat-theme',
    light = 'light',
    dark = 'dark';

function switchTheme(theme, fromEvent = false) {
    document.documentElement.style.setProperty('--background-color', theme === light ? '#FBFEFB' : '#1D2B35');
    document.documentElement.style.setProperty('--text-color', theme === light ? '#1B1F3B' : '#FBFEFB');
    document.documentElement.style.setProperty('--external-accent', theme === light ? '#EFE5DC' : '#274958');
    document.documentElement.style.setProperty('--other-heading-color', theme === light ? '#42858C' : '#64FCD9');

    document.documentElement.style.setProperty('--im-background-color', theme === light ? '#F0E8DD' : '#393E41');
    document.documentElement.style.setProperty('--im-second-background-color', theme === light ? '#B8574F' : '#26292C');
    document.documentElement.style.setProperty('--im-chat-background-image', theme === light ? 'url(/file/default/chat-background-light.svg)' : 'url(/file/default/chat-background-dark.svg)');

    //if the user choose the theme instead of coming from the system preference, save it
    if (fromEvent)
        localStorage.setItem(key, theme);
}

if (localStorage.getItem(key))
    switchTheme(localStorage.getItem(key));
else if (window.matchMedia && window.matchMedia('(prefers-color-scheme: light)').matches)
    switchTheme(light);
else if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches)
    switchTheme(dark);

document.addEventListener("DOMContentLoaded", () => {
    document.querySelector('#light-theme-switch').addEventListener('click', () => switchTheme(light, true));
    document.querySelector('#dark-theme-switch').addEventListener('click', () => switchTheme(dark, true));
});

