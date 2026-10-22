export function documentHasFocus() {
      return typeof document === 'undefined' || document.hasFocus();
}

export function documentIsNotHidden() {
      return typeof document === 'undefined' || !document.hidden;
}
