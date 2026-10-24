import * as P from 'node:path';
import * as FS from 'node:fs';

function InlineResourcePlugin(options = {}) {
    this.options = options;
}

InlineResourcePlugin.prototype.apply = function (compiler) {
    let done = statsData => {
        if (!statsData.hasErrors())
            inline(this.options);
    };

    compiler.hooks.done.tap({ name: 'InlineResourcePlugin' }, done);
};

function inline(options) {
    for (let entry of options.files) {
        let htmlPath = P.resolve(entry.htmlFile),
            htmlContents = FS.readFileSync(htmlPath, 'utf8'),
            resourceName = entry.resourceFile.split('/').pop(),
            resourceContents = FS.readFileSync(P.resolve(entry.resourceFile), 'utf8');

      htmlContents = htmlContents.replaceAll(JSON.stringify(`666 ${resourceName} 666`), () => JSON.stringify(resourceContents));

      FS.writeFileSync(htmlPath, htmlContents);
    }
}

export default InlineResourcePlugin;
