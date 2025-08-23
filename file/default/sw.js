importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js');

/// caching
let { registerRoute, Route } = workbox.routing,
    { CacheFirst } = workbox.strategies,
    { CacheableResponsePlugin } = workbox.cacheableResponse;

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

let chattingWith,
    channel = new BroadcastChannel("merochat");

channel.addEventListener('message', async (event) => {
    if (event.data)
        switch (event.data.type) {
            case 'not-chatting':
                noChatsOpened();
                break;
            case 'read':
                await chatOpened(event.data.payload);
                break;
        }
});

function noChatsOpened() {
    chattingWith = undefined;
}

async function chatOpened(payload) {
    chattingWith = payload.userId;

    await hideNotifications(payload);
    return setBadge();
}

//when opening a chat from the app / receiving a read status push
async function hideNotifications(payload) {
    let notifications = await self.registration.getNotifications({ tag: payload.userId });

    for (let n of notifications)
        n.close();
}

self.addEventListener('push', (event) => {
    event.waitUntil(handlePush(event.data));
});

async function handlePush(raw) {
    let data = raw.json();

    if (data.message) {
        let parsed = JSON.parse(data.message.message);

        switch (parsed.type) {
            case 'incoming':
                return notify(data.message, parsed);
            case 'read':
                await hideNotifications(parsed);
                return setBadge();
        }
    }
}

async function notify(data, incoming) {
    let windows = await self.clients.matchAll({ type: 'window' });

    //do not raise notifications if chat is open
    if (windows.length == 1 && !windows[0].hidden && windows[0].focused && incoming.senderId === chattingWith)
        return;

    await pushNotification(data, incoming);
    return setBadge();
}

let apple = (function () {
    let platform = navigator.platform.toLowerCase();
    return platform.startsWith('mac') || platform.startsWith('iphone') || platform.startsWith('ipad') || platform.startsWith('pike');
})();

async function pushNotification(data, incoming) {
    let body,
        allIncoming = [],
        previousNotifications = await self.registration.getNotifications({ tag: incoming.senderId });

    incoming.content = handleMarkdown(incoming.content);

    if (previousNotifications.length == 0) {
        body = incoming.content;
        allIncoming.push(incoming);
    } else {
        previousNotifications[0].data.allIncoming.push(incoming);
        allIncoming = previousNotifications[0].data.allIncoming;
        //on apple shit notification.tag does not replace previous notifiations so no point in grouping them
        if (apple)
            body = incoming.content;
        else
            body = allIncoming.map(ai => ai.content).join('\n');
    }

    return self.registration.showNotification(data.title, {
        body,
        icon: 'https://mero.chat/file/default/android-launchericon-48-48.png',
        badge: 'https://mero.chat/file/default/badge.png',
        tag: incoming.senderId,
        data: { allIncoming }
    });
}

async function setBadge() {
    if ('setAppBadge' in navigator) {
        let allNotifications = await self.registration.getNotifications();

        return navigator.setAppBadge((new Set(allNotifications.map(n => n.tag))).size);
    }
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
    let windows = await self.clients.matchAll({ type: 'window' }),
        userId = parseInt(notification.tag);

    notification.close();

    if (windows.length > 0) {
        await windows[0].focus();

        //sync messages from here meanwhile the app loads them
        channel.postMessage({ message: 'pushed', payload: notification.data.allIncoming });
        channel.postMessage({ message: 'resume', payload: userId });
    } else {
        noChatsOpened();
        //the promise for openWindow might never resolve (ask the chrome developers) so the url is a hack for resuming into a chat
        await self.clients.openWindow(`/im?resume=${userId}`);
    }
}

//store chats and user data localy with indexeddb?
//this would also require some sort of caching of the im page
// and js/css scripts

