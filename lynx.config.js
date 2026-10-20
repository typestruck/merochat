import { defineConfig } from '@lynx-js/rspeedy'

import { pluginQRCode } from '@lynx-js/qrcode-rsbuild-plugin'
import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin'
import { pluginLynxConfig } from '@lynx-js/config-rsbuild-plugin'

export default defineConfig({
    source: {
        entry: './index.js',
        define: {
            'process.env.PRODUCTION': true,
            //has to be https or localhost
            'process.env.LYNX_API_URL': JSON.stringify(process.env.LYNX_API_URL || 'https://mero.chat'),
            'process.env.VAPID_PUBLIC_KEY': "'something or the other'",
            'process.env.VAPID_PRIVATE_KEY': "'secret something or the other'",
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
        }),
    ],
})
