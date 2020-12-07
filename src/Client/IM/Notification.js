exports.createNotification_ = function (name) {
    new Notification('MelanChat', { tag: name, body: `New message from ${name}`, icon: '/client/media/loading.png' });
}