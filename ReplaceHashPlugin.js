import { resolve } from 'path';
import { readFileSync, readdirSync, writeFileSync } from 'fs';

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
    let hash = new Map()

    hash.set('[VAPID-PUBLIC-KEY-contenthash]', process.env.VAPID_PUBLIC_KEY);

    for (let key in assets) {
        let splitted = key.split('.'),
            extension = splitted.pop();

        if (extension != 'js' && extension != 'css')
            continue;

        hash.set(`[${splitted[0]}-${extension}-contenthash]`, splitted[1]);
    }

    for (let entry of options.files) {
        let fileName;

        if (typeof entry === 'string')
            fileName = entry;
        else {
            fileName = `${entry.dir}/${readdirSync(resolve(entry.dir)).find(f => f.startsWith(entry.prefix) && f.endsWith('.js'))}`;
        }

        replaceInFile(fileName, hash);
    }
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