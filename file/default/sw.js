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
        ]})
    );

registerRoute(imageRoute);

self.addEventListener('install', _ => {});

/// push
self.addEventListener('push', (event) => {
    let payload = event.data?.text();

    event.waitUntil(
        self.registration.showNotification('MeroChat', {
            body: payload,
      })
    );
});


//store chats and user data localy with indexeddb?
    //this would also require some sort of caching of the im page
    // and js/css scripts

