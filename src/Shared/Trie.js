import trieSearch from 'trie-search';

export function makeTrie_(value) {
      var trie = new trieSearch('k', {
            min: 2,
            expandRegexes: [],
            splitOnRegEx: /_/
      });
      trie.addAll(value);

      return trie;
}

export function complete_(trie, word) {
      return trie.get(word);
}