let key = 'merochat-theme';
let light = 'light';
let dark = 'dark';

function switchTheme(theme, fromEvent = false) {
    document.documentElement.style.setProperty('--background-color', theme === light ? '#FBFEFB' : '#1D2B35');
    document.documentElement.style.setProperty('--text-color', theme === light ? '#1B1F3B' : '#FBFEFB');
    document.documentElement.style.setProperty('--external-accent', theme === light ? '#EFE5DC' : '#274958');
    document.documentElement.style.setProperty('--other-heading-color', theme === light ? '#42858C' : '#64FCD9');

    document.documentElement.style.setProperty('--im-background-color', theme === light ? '#DB5A5D' : '#393E41');

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

document.querySelector('#light-theme-switch').addEventListener('click', () => switchTheme(light, true));
document.querySelector('#dark-theme-switch').addEventListener('click', () => switchTheme(dark, true));