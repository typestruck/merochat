//this extra config file exists because i couldn't find how to bundle landing into a single file (without depending on chunks) while using chunk splitting in webpack.production.config.js
import path from 'path';
import webpack from 'webpack';

import CssMinimizerPlugin from 'css-minimizer-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TerserPlugin from 'terser-webpack-plugin';
import ReplaceHashPlugin from './ReplaceHashPlugin.js';
import InlineResourcePlugin from './InlineResourcePlugin.js';

export default {
    mode: 'production',
    entry: {
        landing: './loader/production/landing.bundle.js',
        style: ['./src/Client/css/base.css', './src/Client/css/landing.css']
    },
    output: {
        path: path.resolve('.', 'file/bundle'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].css',
        }),
        new webpack.DefinePlugin({
            'process.env.PRODUCTION': true,
            'process.env.VAPID_PUBLIC_KEY': JSON.stringify(process.env.VAPID_PUBLIC_KEY)
        }),
        new ReplaceHashPlugin({ files: [{ dir: 'file/bundle', prefix: 'common' }, 'output-es/Shared.Resource/index.js'] }),
        new InlineResourcePlugin({ files: [{resourceFile: 'file/bundle/style.css', htmlFile: 'output-es/Server.Landing.Template/index.js'} ]}),
        new InlineResourcePlugin({ files: [{resourceFile: 'file/default/theme-switcher.js', htmlFile: 'output-es/Server.Template/index.js'} ]}),
        new InlineResourcePlugin({ files: [{resourceFile: 'file/default/theme-switcher.js', htmlFile: 'output-es/Server.Im.Template/index.js'} ]}),
    ],
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader',
                        options: {
                            url: false,
                        }
                    }
                ]
            }
        ]
    },
    optimization: {
        removeEmptyChunks: true,
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin(),
        ]
    },

};
