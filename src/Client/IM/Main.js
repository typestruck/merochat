const SimpleMDE = require('simplemde');

exports.loadEditor = function() {
        return new SimpleMDE({
                autofocus: true,
                hideIcons: ['fullscreen', 'guide'],
                status: false
        });
}