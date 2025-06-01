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

export function mediaMatches_(displayMode) {
      return window.matchMedia('(display-mode: ' + displayMode + ')').matches;
}

export function documentHasFocus() {
      return document.hasFocus();
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

export function removeFromClassList_(element, c) {
      element.classList.remove(c);
}

export function addToClassList_(element, c) {
      element.classList.add(c);
}

export function register_(navigator, file) {
      navigator.serviceWorker.register(file);
}

export async function ready_(navigator) {
      return await navigator.serviceWorker.ready;
}

export async function getSubscription_(registration) {
      return await registration.pushManager.getSubscription();
}

export async function subscribe_(registration) {
      let base64String = '[VAPID-PUBLIC-KEY-contenthash]',
            padding = '='.repeat((4 - base64String.length % 4) % 4),
            base64 = (base64String + padding).replace(/\-/g, '+').replace(/_/g, '/'),
            rawData = Buffer.from(base64).toString('base64'),
            vapidPublicKey = new Uint8Array(rawData.length);

      for (let i = 0; i < rawData.length; ++i) {
            vapidPublicKey[i] = rawData.charCodeAt(i);
      }

      await registration.pushManager.subscribe({ userVisibleOnly: true, applicationServerKey: vapidPublicKey });
}

export function isMediaTypeSupported_(mt) {
      return MediaRecorder.isTypeSupported(mt);
}


