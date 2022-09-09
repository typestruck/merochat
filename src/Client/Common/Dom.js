export function innerHTML_(element, html) {
      element.innerHTML = html;
}

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

export function documentHasFocus() {
      return document.hasFocus();
}

export function value_(element) {
      return element.value || element.innerText;
}

export function setValue_(element, value) {
      if (element.value)
            element.value = value;
      else
            element.innerText = value;
}

export function toggleDisabled_(element) {
      element.disabled = !element.disabled;
}

export function screenWidth() {
      return screen.width;
}

export function requestNotificationPermission() {
      Notification.requestPermission();
}

export function notificationPermission() {
      return Notification.permission;
}

export function pushState_(location) {
      history.pushState(null, '', location);
}

export function scrollIntoView_(element) {
      element.scrollIntoView(true);
}