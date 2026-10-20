export function setTheme_(theme) {
    globalThis.switchTheme(theme, true);
}

//defined by the theme-swither until we clean this up
export function getTheme_() {
    return globalThis.themeProperties || {};
}
