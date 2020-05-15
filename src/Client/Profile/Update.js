//PERFORMANCE: load simplemde et al only once
const SimpleMDE = require('simplemde');

exports.setEditorContent_ = function (editor, contents) {
        editor.value(contents);
}

