export function innerText_(element) {
      return element.innerText;
}

export function createCustomEvent_(name, value) {
      return new CustomEvent(name, {
            detail: {
                  value: value
            }
      });
}

export function customEventDetail_(event) {
      return event.detail.value;
}

export function target_(event) {
      let target = event.target;

      return target && typeof target.addEventListener === 'function' ? target : null;
}

export function preventDefault(event) {
      return function () {
            return event.preventDefault?.();
      };
}

export function stopPropagation(event) {
      return function () {
            return event.stopPropagation?.();
      };
}

export function key(event) {
      return event.key ?? event.detail?.key ?? '';
}

export function shiftKey(event) {
      return event.shiftKey ?? event.detail?.shiftKey ?? false;
}

export function files_(event) {
      return event.dataTransfer?.files ?? event.target?.files ?? null;
}
