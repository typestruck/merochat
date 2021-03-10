'use strict';

const path = require('path');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const TerserPlugin = require("terser-webpack-plugin");
//const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

module.exports = {
    mode: 'production',
    entry: {
        im: {
            import: ['./loader/im.bundle.js', './src/Client/css/im.css'],
            dependOn: 'emoji'
        },
        landing: ['./loader/landing.bundle.js', './src/Client/css/landing.css'],
        login: './loader/login.bundle.js',
        profile: ['./loader/profile.bundle.js', './src/Client/css/profile.css'],
        leaderboard: ['./loader/leaderboard.bundle.js', './src/Client/css/leaderboard.css'],
        help: ['./loader/help.bundle.js', './src/Client/css/help.css'],
        internalHelp: './loader/internalHelp.bundle.js',
        settings: ['./loader/settings.bundle.js', './src/Client/css/settings.css'],
        experiments: ['./loader/experiments.bundle.js', './src/Client/css/experiments.css'],
        recover: './loader/recover.bundle.js',
        emoji: './output/Shared.IM.Emoji/index.js',
        base: './src/Client/css/base.css',
        external: './src/Client/css/external.css',
        backer: './src/Client/css/backer.css'
    },
    output: {
        path: path.resolve(__dirname, 'dist/production'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].[contenthash].css',
        }),
      //  new BundleAnalyzerPlugin()
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
            name: 'other',
            cacheGroups: {
                common: {
                    name: 'common',
                    test(module) {
                        return module.resource && /(.*)(Shared\.Types|Client\.Common\.Network|Shared\.Routes)(.*)/.test(module.resource)
                    }
                }
            }
        },
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin(),
        ],
    },

};
