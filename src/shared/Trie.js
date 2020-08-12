var trieSearch = require('trie-search');

exports.makeTrie_ = function(value) {
      var trie = new trieSearch('k', {
            min: 2,
            expandRegexes: [],
            splitOnRegEx: /_/
      });
      trie.addAll(value);

      return trie;
}

exports.complete_ = function(trie, word) {
      return trie.get(word);
}