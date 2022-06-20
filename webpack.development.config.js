'use strict';

import path from 'path';
import webpack from 'webpack';
const isWatch = process.argv.some(a => a === '--watch');

export default {
      mode: 'development',
      devtool: 'eval-source-map',

      entry: {
            im: {
                  import: ['./loader/im.bundle.js'],
                  dependOn: 'emoji'
            },
            landing: ['./loader/landing.bundle.js'],
            login: './loader/login.bundle.js',
            profile: {
                  import: ['./loader/profile.bundle.js'],
                  dependOn: 'im'
            },
            leaderboard: {
                  import: ['./loader/leaderboard.bundle.js'],
                  dependOn: 'im'
            },
            help: ['./loader/help.bundle.js'],
            internalHelp: {
                  import: ['./loader/internalHelp.bundle.js'],
                  dependOn: 'im'
            },
            settings: {
                  import: ['./loader/settings.bundle.js'],
                  dependOn: 'im'
            },
            experiments: {
                  import: ['./loader/experiments.bundle.js'],
                  dependOn: 'im'
            },
            recover: './loader/recover.bundle.js',
            emoji: './output/Shared.IM.Emoji/index.js'
      },

      output: {
            path: path.resolve(".", './dist/development'),
            filename: '[name].bundle.js'
      },

      module: {
            rules: [{
                  test: /\.purs$/,
                  use: [{
                        loader: 'purs-loader',
                        options: {
                              src: ['src/**/*.purs'],
                              spago: true,
                              watch: isWatch,
                              pscIde: true
                        }
                  }]
            }]
      },

      resolve: {
            modules: ['node_modules'],
            extensions: ['.purs', '.js']
      },

      optimization: {
            moduleIds: 'deterministic',
            splitChunks: {
                  chunks: 'all',
                  name: 'common'
            },
            minimize: false
      },


      plugins: [
            new webpack.LoaderOptionsPlugin({
                  debug: true
            })
      ]
};