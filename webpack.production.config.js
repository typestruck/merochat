'use strict';

const path = require('path');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const TerserPlugin = require("terser-webpack-plugin");

module.exports = {
    mode: 'production',
    optimization: {
        usedExports: true
    },
    entry: {
        im: ['./loader/im.bundle.js', './src/Client/css/im.css'],
        landing: ['./loader/landing.bundle.js', './src/Client/css/base.css', './src/Client/css/external.css'],
        login: './loader/login.bundle.js',
        profile: ['./loader/profile.bundle.js', './src/Client/css/profile.css'],
        leaderboard: ['./loader/leaderboard.bundle.js', './src/Client/css/profile.css'],
        help: ['./loader/help.bundle.js', './src/Client/css/help.css'],
        internalHelp: './loader/internalHelp.bundle.js',
        settings: ['./loader/settings.bundle.js', './src/Client/css/settings.css'],
        recover: './loader/recover.bundle.js',
    },

    output: {
        path: path.resolve(__dirname, 'dist/production'),
        filename: '[name].[contenthash].min.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].[contenthash].css',
        })
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
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin(),
        ],
    },

};
