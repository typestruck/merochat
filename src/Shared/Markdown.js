var marked = require('marked');

function defaultOptions() {
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
}

function restrictedoptions() {
      marked.use({
            renderer: {
                  link(_, title, text) {
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
}

exports.parse = function (plainMarkdown) {
      defaultOptions();

      return marked(plainMarkdown, {
            smartypants: true
      });
}

exports.parseRestricted = function(plainMarkdown) {
     restrictedoptions();

      return marked(plainMarkdown, {
            smartypants: true
      });
}