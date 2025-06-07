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

    //old notifications get replaced if tag matches
    let body,
        count,
        incoming = JSON.parse(data.message.message),
        previousNotifications = await self.registration.getNotifications({ tag: incoming.senderId });

    if (previousNotifications.length == 0) {
        count = 1;
        body = incoming.content;
    } else {
        count = previousNotifications[0].data + 1;
        body = `${count} messages`;
    }

    return self.registration.showNotification(data.message.title, {
        body,
        icon: 'https://mero.chat/file/default/android-launchericon-48-48.png',
        badge: 'https://mero.chat/file/default/badge.png',
        tag: incoming.senderId,
        data: count
    });
}

self.addEventListener('notificationclick', (event) => {
    event.waitUntil(resume(event.notification));
});

async function resume(notification) {
    let pwa,
        windows = await clients.matchAll({ type: 'window' });

    if (windows.length > 0) {
        pwa = windows[0];
        await pwa.focus();
    } else
        pwa = await clients.openWindow('/im');

    pwa.postMessage({ message: 'resume', userId: parseInt(notification.tag) });

    return Promise.resolve();
}

//store chats and user data localy with indexeddb?
//this would also require some sort of caching of the im page
// and js/css scripts

