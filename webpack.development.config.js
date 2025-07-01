'use strict';

import path from 'path';
import webpack from 'webpack';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';

const isWatch = process.argv.some(a => a === '--watch');

export default {
      mode: 'development',
      devtool: 'eval-source-map',

      entry: {
            im: {
                  import: ['./loader/development/im.bundle.js', './src/Client/css/im.css'],
                  dependOn: 'emoji'
            },
            landing: ['./loader/development/landing.bundle.js', './src/Client/css/landing.css'],
            login: './loader/development/login.bundle.js',
            profile: {
                  import: ['./loader/development/profile.bundle.js', './src/Client/css/profile.css'],
                  dependOn: 'im'
            },
            karmaPrivileges: {
                  import: ['./loader/development/karmaPrivileges.bundle.js', './src/Client/css/karmaPrivileges.css'],
                  dependOn: 'im'
            },
            help: ['./loader/development/help.bundle.js', './src/Client/css/help.css'],
            internalHelp: {
                  import: ['./loader/development/internalHelp.bundle.js'],
                  dependOn: 'im'
            },
            settings: {
                  import: ['./loader/development/settings.bundle.js', './src/Client/css/settings.css'],
                  dependOn: 'im'
            },
            experiments: {
                  import: ['./loader/development/experiments.bundle.js', './src/Client/css/experiments.css'],
                  dependOn: 'im'
            },
            feedback: {
                  import: ['./loader/development/feedback.bundle.js',  './src/Client/css/feedback.css'],
                  dependOn: 'im'
            },
            recover: './loader/development/recover.bundle.js',
            emoji: './output/Shared.Im.Emoji/index.js',
            base: './src/Client/css/base.css',
            backer: './src/Client/css/backer.css',
            internalBacker: {
                  import: ['./loader/development/internalBacker.bundle.js'],
                  dependOn: 'im'
            }
      },

      output: {
            path: path.resolve(".", './file/bundle'),
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
            },
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
            new MiniCssExtractPlugin({
                  filename: '[name].css',
            }),
            new webpack.LoaderOptionsPlugin({
                  debug: true
            })
            ,
            new webpack.DefinePlugin({
                  'process.env.PRODUCTION': false
            })
      ]
};