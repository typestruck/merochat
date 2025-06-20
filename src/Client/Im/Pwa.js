
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

export function subscribe_(registration, cb) {
    let vp = '[VAPID-PUBLIC-KEY-contenthash]',
        padding = '=',
        base64 = vp.replace(/\-/g, '+').replace(/_/g, '/') + padding,
        rawData = window.atob(base64),
        vapidPublicKey = new Uint8Array(rawData.length);

    for (let i = 0; i < rawData.length; ++i) {
        vapidPublicKey[i] = rawData.charCodeAt(i);
    }

    registration.pushManager.subscribe({ userVisibleOnly: true, applicationServerKey: vapidPublicKey }).then(s => {
        cb(s)();
    });
}

export function topicBody_(subscription, topic) {
    let serializedSubscription = JSON.parse(JSON.stringify(subscription));

    return JSON.stringify(
        {
            endpoint: serializedSubscription.endpoint,
            auth: serializedSubscription.keys.auth,
            p256dh: serializedSubscription.keys.p256dh,
            topics: [topic],
        });
}

let channel = new BroadcastChannel("merochat");

export function receiveMessage_(cb) {
    channel.addEventListener("message", (event) => {
        cb(event.data.message)(event.data.payload)();
    });
}

export function postMessage_(type, payload) {
    channel.postMessage({
        type,
        payload
    });
}