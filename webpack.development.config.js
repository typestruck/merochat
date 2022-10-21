'use strict';

import path from 'path';
import webpack from 'webpack';
const isWatch = process.argv.some(a => a === '--watch');

export default {
      mode: 'development',
      devtool: 'eval-source-map',

      entry: {
            im: {
                  import: ['./loader/development/im.bundle.js'],
                  dependOn: 'emoji'
            },
            landing: ['./loader/development/landing.bundle.js'],
            login: './loader/development/login.bundle.js',
            profile: {
                  import: ['./loader/development/profile.bundle.js'],
                  dependOn: 'im'
            },
            karmaPrivileges: {
                  import: ['./loader/development/karmaPrivileges.bundle.js'],
                  dependOn: 'im'
            },
            help: ['./loader/development/help.bundle.js'],
            internalHelp: {
                  import: ['./loader/development/internalHelp.bundle.js'],
                  dependOn: 'im'
            },
            settings: {
                  import: ['./loader/development/settings.bundle.js'],
                  dependOn: 'im'
            },
            experiments: {
                  import: ['./loader/development/experiments.bundle.js'],
                  dependOn: 'im'
            },
            feedback: {
                  import: ['./loader/development/feedback.bundle.js'],
                  dependOn: 'im'
            },
            recover: './loader/development/recover.bundle.js',
            emoji: './output/Shared.Im.Emoji/index.js'
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
            ,
            new webpack.DefinePlugin({
                  'process.env.PRODUCTION': false
            })
      ]
};