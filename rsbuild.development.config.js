import * as RC from './rsbuild.shared.config.js';

export default RC.createConfig({
    production: false,
    landing: false,
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
    plugins: []
});
