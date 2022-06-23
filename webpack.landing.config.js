'use strict';

import path from 'path';
import webpack from 'webpack';

import CssMinimizerPlugin from 'css-minimizer-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import TerserPlugin from "terser-webpack-plugin";

export default {
    mode: 'production',
    entry: {
        landing: './loader/landing.bundle.js',
        style: ['./src/Client/css/base.css', './src/Client/css/external.css', './src/Client/css/landing.css']
    },
    output: {
        path: path.resolve(".", 'dist/production'),
        filename: '[name].[contenthash].bundle.js'
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].css',
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
        ]
    },

};
