let key = 'merochat-theme';

function switchTheme(light) {
    document.documentElement.style.setProperty('--background-color', light ? '#FBFEFB' : '#1D2B35');
    document.documentElement.style.setProperty('--text-color', light ? '#1B1F3B' : '#FBFEFB');
    document.documentElement.style.setProperty('--external-accent', light ? '#EFE5DC' : '#274958');
    document.documentElement.style.setProperty('--other-heading-color', light ? '#42858C' : '#64FCD9');

    localStorage.setItem(key, light ? 'light' : 'dark');
}

if (localStorage.getItem(key) === 'light')
    switchTheme(true);
else if (localStorage.getItem(key) === 'dark')
    switchTheme(false);


document.querySelector('#light-theme-switch').addEventListener('click', () => switchTheme(true));
document.querySelector('#dark-theme-switch').addEventListener('click', () => switchTheme(false));