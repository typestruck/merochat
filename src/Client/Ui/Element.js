function invoke(element, method, params) {
      return new Promise((resolve, reject) => {
            lynx.createSelectorQuery()
                  .select(element.lynxSelector)
                  .invoke({
                        method,
                        params,
                        success: resolve,
                        fail: error => reject(new Error(`Element ${element.lynxSelector} ${method} failed: ${JSON.stringify(error)}`))
                  })
                  .exec();
      });
}

export function innerHTML_(element, html) {
      element.innerHTML = html;
}

export function removeFromClassList_(element, c) {
      element.classList.remove(c);
}

export function addToClassList_(element, c) {
      element.classList.add(c);
}

export function value_(element) {
      if (element.lynxSelector !== undefined)
            return invoke(element, 'getValue', {}).then(result => result.value);

      return Promise.resolve(element.value ?? element.innerText);
}

export function setValue_(element, value) {
      if (element.lynxSelector !== undefined)
            return invoke(element, 'setValue', { value });

      element.value = value;

      return Promise.resolve();
}

export function toggleDisabled_(element) {
      element.disabled = !element.disabled;
}

export function focus_(element) {
      if (element.lynxSelector !== undefined)
            return invoke(element, 'focus', {});

      element.focus();

      return Promise.resolve();
}

export function selectionEnd_(element) {
      if (element.lynxSelector !== undefined)
            return invoke(element, 'getValue', {}).then(result => result.selectionEnd);

      return Promise.resolve(element.selectionEnd);
}

export function setSelection_(element, position) {
      if (element.lynxSelector !== undefined)
            return invoke(element, 'setSelectionRange', { selectionStart: position, selectionEnd: position });

      element.setSelectionRange(position, position);

      return Promise.resolve();
}
