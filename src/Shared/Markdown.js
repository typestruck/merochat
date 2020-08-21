var marked = require('marked');

//REFACTOR: make this all type safe
exports.parse = function (input) {
      marked.use({
            renderer: {
                  link(href, title, text) {
                        if (!href.includes('://'))
                              href = 'http://' + href;

                        return `<a href="${href}" title="${title || ""}" target="blank">${text}</a>`;
                  },
                  image(src, title, text) {
                        var tag = `<img src="${src}" alt="${title || ""}" />`

                        if (text)
                              tag += `<br/>${text}`;

                        return tag;
                  }
            }
      });

      return marked(input, {
            smartypants: true
      });
}

exports.parseRestricted = function(input) {
      marked.use({
            renderer: {
                  link(href, title, text) {
                        return `<a title="${title || ""}">${text}</a>`;
                  },
                  image(_, title) {
                        var tag = '<br/>You sent an image';
                        if (title)
                              tag = `${tag}: ${title}`;

                        return tag;
                  }
            }
      });
      return marked(input, {
            smartypants: true
      });
}