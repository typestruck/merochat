import { resolve } from 'path';
import { readFileSync, writeFileSync } from 'fs';

function InlineStylePlugin(options = {}) {
    this.options = options;
}

InlineStylePlugin.prototype.apply = function (compiler) {
    let done = statsData => {
        if (!statsData.hasErrors())
            inline(this.options);
    };

    compiler.hooks.done.tap({ name: 'InlineStylePlugin' }, done);
};

function inline(options) {
    for (let entry of options.files) {
        let htmlPath = resolve(entry.htmlFile),
            htmlContents = readFileSync(htmlPath, 'utf8'),
            styleName = entry.styleFile.split('/').pop(),
            styleContents = readFileSync(resolve(entry.styleFile), 'utf8').replaceAll('"', "'");

      htmlContents = htmlContents.replace(`<% ${styleName} %>`, `${styleContents}`);

      writeFileSync(htmlPath, htmlContents);
    }
}

export default InlineStylePlugin;