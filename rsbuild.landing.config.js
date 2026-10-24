import * as RC from './rsbuild.shared.config.js';
import RHP from './ReplaceHashPlugin.js';
import IRP from './InlineResourcePlugin.js';

export default RC.createConfig({
    production: true,
    landing: true,
    entry: {
        landing: './loader/production/landing.bundle.js',
        style: ['./src/Client/css/base.css', './src/Client/css/landing.css']
    },
    plugins: [
            new RHP({ files: ['output-es/Shared.Resource/index.js'] }),
            new IRP({ files: [
                { resourceFile: 'file/bundle/style.css', htmlFile: 'output-es/Server.Landing.Template/index.js' },
                { resourceFile: 'file/default/theme-switcher.js', htmlFile: 'output-es/Server.Template/index.js' },
                { resourceFile: 'file/default/theme-switcher.js', htmlFile: 'output-es/Server.Im.Template/index.js' }
            ] })
        ]
});
