import * as RC from './rsbuild.shared.config.js';
import RHP from './ReplaceHashPlugin.js';

export default RC.createConfig({
    production: true,
    landing: false,
    entry: {
        im: {
            import: ['./loader/production/im.bundle.js', './src/Client/css/im.css'],
            dependOn: 'emoji'
        },
        login: './loader/production/login.bundle.js',
        profile: {
            import: ['./loader/production/profile.bundle.js', './src/Client/css/profile.css'],
            dependOn: 'im'
        },
        karmaPrivileges: {
            import: ['./loader/production/karmaPrivileges.bundle.js', './src/Client/css/karmaPrivileges.css'],
            dependOn: 'im'
        },
        internalBacker: {
            import: './loader/production/internalBacker.bundle.js',
            dependOn: 'im'
        },
        help: ['./loader/production/help.bundle.js', './src/Client/css/help.css'],
        internalHelp: {
            import: './loader/production/internalHelp.bundle.js',
            dependOn: 'im'
        },
        settings: {
            import: ['./loader/production/settings.bundle.js', './src/Client/css/settings.css'],
            dependOn: 'im'
        },
        experiments: {
            import: ['./loader/production/experiments.bundle.js', './src/Client/css/experiments.css'],
            dependOn: 'im'
        },
        feedback: {
            import: ['./loader/production/feedback.bundle.js', './src/Client/css/feedback.css'],
            dependOn: 'im'
        },
        recover: './loader/production/recover.bundle.js',
        emoji: './output-es/Shared.Im.Emoji/index.js',
        base: './src/Client/css/base.css',
        backer: './src/Client/css/backer.css'
    },
    plugins: [new RHP({ files: [{ dir: 'file/bundle', prefix: 'common' }, 'output-es/Shared.Resource/index.js'] })]
});
