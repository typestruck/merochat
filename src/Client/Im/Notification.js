export function createNotification_(options) {
    let n = new Notification('MeroChat', { body: options.body, icon: options.icon });

    n.onclick = function() {
        options.handler();
    };
}