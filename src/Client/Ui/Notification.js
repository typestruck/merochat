export async function requestNotificationPermission(cb) {
      let status = await Notification.requestPermission();

      if (status == 'granted')
            cb()();
}

export function notificationPermission() {
      return Notification.permission;
}
