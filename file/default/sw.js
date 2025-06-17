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
        windows = await self.clients.matchAll({ type: 'window' });

    //do not raise notifications if app is open
    if (windows.length == 1 && !windows[0].hidden && windows[0].focused || !data.message)
        return Promise.resolve();

    //old notifications get replaced if tag matches (except on safari)
    let body,
        allIncoming = [],
        incoming = JSON.parse(data.message.message),
        previousNotifications = await self.registration.getNotifications({ tag: incoming.senderId });

    incoming.content = handleMarkdown(incoming.content);

    if (previousNotifications.length == 0) {
        body = incoming.content;
        allIncoming.push(incoming);
    } else {
        previousNotifications[0].data.allIncoming.push(incoming);
        allIncoming = previousNotifications[0].data.allIncoming;
        body = allIncoming.map(ai => ai.content).join('\n');
    }

    return self.registration.showNotification(data.message.title, {
        body,
        icon: 'https://mero.chat/file/default/android-launchericon-48-48.png',
        badge: 'https://mero.chat/file/default/badge.png',
        tag: incoming.senderId,
        data: { allIncoming }
    });
}

//avoid markdown only messages but ignore formatting inside for now
function handleMarkdown(raw) {
    let firstCharacter = raw[0];

    switch (firstCharacter) {
        case '>':
            return 'Quote';
        case '!':
            return 'Picture';
        case '<':
            return 'Audio';
        case '[':
            return 'Link';
        default:
            return raw;
    }
}

self.addEventListener('notificationclick', (event) => {
    event.waitUntil(resume(event.notification));
});

async function resume(notification) {
    let pwa,
        windows = await self.clients.matchAll({ type: 'window' }),
        userId = parseInt(notification.tag);

    notification.close();

    if (windows.length > 0) {
        pwa = windows[0];
        await pwa.focus();

        //sync messages from here meanwhile the app loads them
        pwa.postMessage({ message: 'pushed', payload: notification.data.allIncoming });
        pwa.postMessage({ message: 'resume', payload: userId });
    } else
        //the promise for openWindow might never resolve (ask the chrome developers) so the url is a hack for resuming into a chat
        pwa = await self.clients.openWindow(`/im?resume=${userId}`);

    return Promise.resolve();
}

self.addEventListener('message', (event) => {
    if (event.data && event.data.type === 'read')
        waitUntil(hideNotifications(event.data.payload))
});

//when opening a chat from the app / receiving a read status push
async function hideNotifications(tag) {
    let notifications = await self.registration.getNotifications({ tag });

    for (let n of notifications)
        n.close();

    return Promise.resolve();
}

//store chats and user data localy with indexeddb?
//this would also require some sort of caching of the im page
// and js/css scripts

