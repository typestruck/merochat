'use strict';

import path from 'path';
import CssMinimizerPlugin from 'css-minimizer-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TerserPlugin from "terser-webpack-plugin";

export default {
    mode: 'production',
    entry: {
        im: {
            import: ['./loader/im.bundle.js', './src/Client/css/im.css'],
            dependOn: 'emoji'
        },
        login: './loader/login.bundle.js',
        profile: {
            import: ['./loader/profile.bundle.js', './src/Client/css/profile.css'],
            dependOn: 'im'
        },
        leaderboard: {
            import: ['./loader/leaderboard.bundle.js', './src/Client/css/leaderboard.css'],
            dependOn: 'im'
        },
        help: ['./loader/help.bundle.js', './src/Client/css/help.css'],
        internalHelp: {
            import: './loader/internalHelp.bundle.js',
            dependOn: 'im'
        },
        settings: {
            import: ['./loader/settings.bundle.js', './src/Client/css/settings.css'],
            dependOn: 'im'
        },
        experiments: {
            import: ['./loader/experiments.bundle.js', './src/Client/css/experiments.css'],
            dependOn: 'im'
        },
        recover: './loader/recover.bundle.js',
        emoji: './output/Shared.IM.Emoji/index.js',
        base: './src/Client/css/base.css',
        external: './src/Client/css/external.css',
        backer: './src/Client/css/backer.css'
    },
    output: {
        path: path.resolve(".", 'dist/production'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].[contenthash].css',
        }),
        // new BundleAnalyzerPlugin()
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
        moduleIds: 'deterministic',
        splitChunks: {
            chunks: 'all',
            name: 'common'
        },
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin(),
        ]
    },

};
