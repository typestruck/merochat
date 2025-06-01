
export function register_(navigator, file) {
    navigator.serviceWorker.register(file, {
        scope: '/',
    });
}

export function ready_(navigator, cb) {
    navigator.serviceWorker.ready.then(r => {
        cb(r)();
    }).catch(e => {
        console.error(e);
    });
}

export function getSubscription_(registration, cb) {
    registration.pushManager.getSubscription().then(s => {
        cb(s)();
    });
}

export async function subscribe_(registration) {
    let vp = '[VAPID-PUBLIC-KEY-contenthash]',
        padding = '=',
        base64 = (vp + padding).replace(/\-/g, '+').replace(/_/g, '/'),
        vapidPublicKey = Uint8Array.fromBase64(base64)
        sub = registration.pushManager.subscribe({ userVisibleOnly: true, applicationServerKey: vapidPublicKey });

    return sub;
}