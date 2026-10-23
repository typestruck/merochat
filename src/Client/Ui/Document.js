export function documentHasFocus() {
      return typeof document === 'undefined' || document.hasFocus();
}

export function documentIsNotHidden() {
      return typeof document === 'undefined' || !document.hidden;
}

export function unsafeGetElementById_(id) {
      if (typeof lynx !== 'undefined')
            return { lynxSelector: `#${id}` };

      let element = document.getElementById(id);

      if (!element)
            throw new Error(`No element with id:${id}`);

      return element;
}
