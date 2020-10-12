var marked = require('marked');

//REFACTOR: make this all type safe
function markdown(plainMarkdown, use) {
      return function(oldVnode, vnode) {
            var oldPlainMarkdown = oldVnode.plainMarkdown

            vnode.plainMarkdown = plainMarkdown;

            if (oldPlainMarkdown != plainMarkdown) {
                  use();
                  vnode.elm.innerHTML = marked(plainMarkdown, {
                        smartypants: true
                  });
            }
      }
}

exports.parseHook = function (plainMarkdown) {
      return markdown(plainMarkdown, defaultOptions);
}

exports.parseRestrictedHook = function(plainMarkdown) {
      return markdown(plainMarkdown, restrictedoptions);
}

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