const SimpleMDE = require('simplemde');

exports.loadEditor = function() {
      return new SimpleMDE({
            autofocus: true,
            hideIcons: ['image', 'side-by-side', 'fullscreen', 'guide'],
            status: false
      });
}

exports.keyHandled_ = function(editor, handler) {
      editor.codemirror.on('keyHandled', function(_, name, __) {
            if (name == 'Enter' && editor.value()) {
                  handler(editor.value());
                  editor.value("");
            }
      });
}
