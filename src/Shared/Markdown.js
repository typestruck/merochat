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

exports.parse = function (plainMarkdown) {
     return markdown(plainMarkdown, function () {
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
      });
}

exports.parseRestricted = function(plainMarkdown) {
      return markdown(plainMarkdown, function () {
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
      });
}