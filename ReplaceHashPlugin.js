import { resolve } from 'path';
import { readFileSync, writeFileSync } from 'fs';

function ReplaceHashPlugin(options = {}) {
    this.options = options;
}

ReplaceHashPlugin.prototype.apply = function (compiler) {
    let done = statsData => {
        if (!statsData.hasErrors())
            replace(this.options, statsData.compilation.assets);
    };

    compiler.hooks.done.tap({ name: 'ReplaceHashPlugin' }, done);
};

function replace(options, assets) {
    let hash = new Map(),
        target;

    for (let key in assets) {
        let splitted = key.split('.'),
            extension = splitted.pop();

        if (extension != 'js' && extension != 'css')
            continue;

        if (key.startsWith(options.filePrefix))
            target = key;

        hash.set(`[${splitted[0]}-${extension}-contenthash]`, splitted[1]);
    }

    replaceInFile(`${options.dir}/${target}`, hash);
    replaceInFile(`${options.file}`, hash);
}

function replaceInFile(fileName, hash) {
      let path = resolve(fileName),
          contents = readFileSync(path, 'utf8');

    for (let [key, value] of hash) {
        contents = contents.replace(key, value);
    }

    writeFileSync(path, contents);
}

export default ReplaceHashPlugin;