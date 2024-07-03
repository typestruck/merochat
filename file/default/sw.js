self.addEventListener('install', (e) => {
    e.waitUntil((async () => {
        // let cache = await caches.open(cacheName);

        // await cache.addAll(contentToCache);
    })());
});