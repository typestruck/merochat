export function createConfig({ production, landing, entry, plugins }) {
    return {
        mode: production ? 'production' : 'development',
        source: {
            entry,
            define: {
                'process.env.PRODUCTION': JSON.stringify(production),
                'process.env.VAPID_PUBLIC_KEY': JSON.stringify(production ? process.env.VAPID_PUBLIC_KEY : 'something or the other') ?? 'undefined',
                'process.env.VAPID_PRIVATE_KEY': JSON.stringify(production ? process.env.VAPID_PRIVATE_KEY : 'secret something or the other') ?? 'undefined'
            }
        },
        output: {
            distPath: { root: 'file/bundle', js: '', css: '' },
            filename: {
                js: production ? '[name].[contenthash].bundle.js' : '[name].bundle.js',
                css: production && !landing ? '[name].[contenthash].css' : '[name].css'
            },
            cleanDistPath: false,
            polyfill: 'off',
            minify: production,
            sourceMap: { js: production ? false : 'eval-source-map', css: false }
        },
        tools: {
            htmlPlugin: false,
            cssLoader: { url: false },
            rspack: config => {
                config.optimization.runtimeChunk = false;
                config.optimization.moduleIds = 'deterministic';
                config.optimization.splitChunks = landing ? false : { chunks: 'all', name: 'common' };
                config.plugins.push(...plugins);
            }
        }
    };
}
