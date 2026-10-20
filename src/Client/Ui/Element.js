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
      return element.value || element.innerText;
}

export function setValue_(element, value) {
      element.value = value;
}

export function toggleDisabled_(element) {
      element.disabled = !element.disabled;
}
