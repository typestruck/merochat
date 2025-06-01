
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
        base64 = vp.replace(/\-/g, '+').replace(/_/g, '/') + padding,
        rawData = window.atob(base64),
        vapidPublicKey = new Uint8Array(rawData.length);

    for (let i = 0; i < rawData.length; ++i) {
        vapidPublicKey[i] = rawData.charCodeAt(i);
    }

    let sub = registration.pushManager.subscribe({ userVisibleOnly: true, applicationServerKey: vapidPublicKey });

    return sub;
}