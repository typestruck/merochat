import webPush from 'web-push';

webPush.setVapidDetails(
    'mailto:contact@mero.chat',
    process.env.VAPID_PUBLIC_KEY,
    process.env.VAPID_PRIVATE_KEY
);

export function push_(subscription, title, message) {
    webPush.sendNotification(JSON.parse(subscription), JSON.stringify({ title, message })).catch(e => console.log(e));
}