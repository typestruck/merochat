importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js');

/// caching
let { registerRoute, Route } = workbox.routing;
let { CacheFirst } = workbox.strategies;
let { CacheableResponsePlugin } = workbox.cacheableResponse;

//cache pictures
let imageRoute =
    new Route(
        ({ request }) => request.destination === 'image',
        new CacheFirst({
            cacheName: 'mero-pics',
            plugins: [
                new CacheableResponsePlugin({
                    statuses: [200]
                })
            ]
        })
    );

registerRoute(imageRoute);

self.addEventListener('install', _ => { });

/// push
self.addEventListener('push', (event) => {
    event.waitUntil(notify(event.data));
});

async function notify(raw) {
    let data = raw.json(),
        windows = await clients.matchAll({ type: 'window' });

    //do not raise notifications if app is open
    if (windows.length == 1 && !windows[0].hidden && windows[0].focused || !data.message)
        return Promise.resolve();

    return self.registration.showNotification(data.message.title, {
        body: 'Message',
        icon: 'https://mero.chat/file/default/loading.png',
        badge: 'https://mero.chat/file/default/badge.png',
        tag: data.message.click,
        data: parseInt(data.message.click)
    })
}

self.addEventListener('notificationclick', (event) => {
    event.notification.close();
    event.waitUntil(resume());
});

async function resume() {
    let windows = await clients.matchAll({ type: 'window' });

    for (let w in windows)
        return w.focus();

    return clients.openWindow('/im');
}

//store chats and user data localy with indexeddb?
//this would also require some sort of caching of the im page
// and js/css scripts

