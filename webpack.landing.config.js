// this config exists because i couldn't find how to bundle landing into a single file (without depending on chunks) while using chunk splitting in webpack.production.config.js

import path from 'path';
import webpack from 'webpack';

import CssMinimizerPlugin from 'css-minimizer-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TerserPlugin from 'terser-webpack-plugin';
import ReplaceHashPlugin from './ReplaceHashPlugin.js';
import InlineStylePlugin from './InlineStylePlugin.js';

export default {
    mode: 'production',
    entry: {
        landing: './loader/production/landing.bundle.js',
        style: ['./src/Client/css/base.css', './src/Client/css/external.css', './src/Client/css/landing.css']
    },
    output: {
        path: path.resolve('.', 'dist/production'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].css',
        }),
        new webpack.DefinePlugin({
            'process.env.PRODUCTION': true
        }),
        new ReplaceHashPlugin({ files: [{ dir: 'dist/production', prefix: 'common' }, 'output-es/Shared.Resource/index.js'] }),
        new InlineStylePlugin({ files: [{styleFile: 'dist/production/style.css', htmlFile: 'output-es/Server.Landing.Template/index.js'} ]})
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
