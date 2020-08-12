function newEditor(selector) {
        return new SimpleMDE({
                element: document.querySelector(selector),
                toolbar: false,
                status: false,
                spellChecker:false
        });
}

function newEditorEnter(selector) {
        var editor = newEditor(selector);

        editor.codemirror.setOption('extraKeys', {
                Enter: function(instance) {
                        instance.display.input.blur();
                }
        });

        return editor;
}

exports.loadEditors = function() {
        return {
                name: newEditorEnter('#profile-edition-name'),
                headline: newEditorEnter('#profile-edition-headline'),
                description: newEditor('#profile-edition-description')
        };
}

exports.blur_ = function(editor, handler) {
        editor.codemirror.on('blur', function() {
                handler(editor.value());
        });
}