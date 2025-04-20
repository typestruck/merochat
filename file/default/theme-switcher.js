
function switchTheme(light) {
    document.documentElement.style.setProperty('--background-color', light ? '#FBFEFB' : '#1D2B35');
    document.documentElement.style.setProperty('--text-color', light ? '#1B1F3B' : '#FBFEFB');
    document.documentElement.style.setProperty('--external-accent', light ? '#EFE5DC' : '#274958');
    document.documentElement.style.setProperty('--other-heading-color', light ? '#42858C' : '#64FCD9');

}

document.querySelector('#light-theme-switch').addEventListener('click', () => switchTheme(true));
document.querySelector('#dark-theme-switch').addEventListener('click', () => switchTheme(false));