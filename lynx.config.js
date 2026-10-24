import { defineConfig } from '@lynx-js/rspeedy'

import { pluginQRCode } from '@lynx-js/qrcode-rsbuild-plugin'
import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin'
import { pluginLynxConfig } from '@lynx-js/config-rsbuild-plugin'

let apiUrl = process.env.LYNX_API_URL || 'http://127.0.0.1:8000';
let websocketPort = Number(process.env.LYNX_WEBSOCKET_PORT || 1339);

function websocketUrl() {
    let api = new URL(apiUrl);

    api.protocol = api.protocol === 'https:' ? 'wss:' : 'ws:';
    if (api.protocol === 'ws:')
        api.port = String(websocketPort);

    return process.env.LYNX_WEBSOCKET_URL || api.toString();
}

export default defineConfig({
    source: {
        entry: './index.js',
        define: {
            'process.env.LYNX_WEBSOCKET_URL': JSON.stringify(websocketUrl()),
            'process.env.LYNX_WEBSOCKET_PORT': websocketPort,
            'process.env.LYNX_API_URL': JSON.stringify(apiUrl),
            'process.env.VAPID_PUBLIC_KEY': "'something or the other'",
        },
    },
    plugins: [
        pluginLynxConfig({
            enableCSSRule: true,
            enableCSSInlineVariables: true,
        }),
        pluginQRCode({
            schema(url) {
                return `${url}?fullscreen=true`
            },
        }),
        pluginReactLynx({
            engineVersion: '4.0',
            enableCSSInheritance: true,
        }),
    ],
})
