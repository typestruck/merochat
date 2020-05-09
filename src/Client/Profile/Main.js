//we probably dont want to blunde simplemde here twice on top of the im bundle
const SimpleMDE = require('simplemde');

exports.loadEditor = function() {
        return new SimpleMDE({
                element: document.querySelector('#profile-edition-description'),
                toolbar: false,
                hideIcons: ['fullscreen', 'guide'],
                status: false,
                spellChecker:false
        });
}

exports.blur_ = function(editor, handler) {
        editor.codemirror.on('blur', function(_, __) {
                handler(editor.value());
        });
}