export function pushState_(location) {
      history.pushState(null, '', location);
}

export function screenWidth() {
      return screen.width;
}

export function mediaMatches_(displayMode) {
      return window.matchMedia('(display-mode: ' + displayMode + ')').matches;
}
