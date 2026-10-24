export let production = process.env.PRODUCTION;
export let vapidPublicKey = process.env.VAPID_PUBLIC_KEY;
export let apiUrl = process.env.LYNX_API_URL || '';
export let websocketUrl = process.env.LYNX_WEBSOCKET_URL;
export let websocketPort = process.env.LYNX_WEBSOCKET_PORT;
