import { resolve } from 'path';
import { readFileSync, writeFileSync } from 'fs';

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
        let htmlPath = resolve(entry.htmlFile),
            htmlContents = readFileSync(htmlPath, 'utf8'),
            resourceName = entry.resourceFile.split('/').pop(),
            resourceContents = readFileSync(resolve(entry.resourceFile), 'utf8').replaceAll('"', "'");

      htmlContents = htmlContents.replace(`"666 ${resourceName} 666"`, `\`${resourceContents}\``);

      writeFileSync(htmlPath, htmlContents);
    }
}

export default InlineResourcePlugin;