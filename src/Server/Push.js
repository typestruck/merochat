import webPush from 'web-push';

webPush.setVapidDetails(
    'mailto:contact@mero.chat',
    process.env.VAPID_PUBLIC_KEY,
    process.env.VAPID_PRIVATE_KEY
);

export function push_(subscription) {
    return function (title) {
        return function (message) {
            return function (onError, onSuccess) {
                webPush.sendNotification(JSON.parse(subscription), JSON.stringify({ title, message }))
                    .then(() => onSuccess(null))
                    .catch((error) => {
                        if (error.statusCode == 410)
                            onError(subscription);
                        else {
                            console.log(error);
                            onError(null);
                        }
                    });

                return function (cancelError, onCancelerError, onCancelerSuccess) {
                    cancel();
                    onCancelerSuccess();
                };
            }
        }
    }
}