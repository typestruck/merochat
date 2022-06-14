import {marked} from 'marked';

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
                  },
                  blockquote(q) {
                        return `<blockquote>${q}</blockquote>`;
                  }
            }
      });
}

function restrictedOptions() {
      marked.use({
            renderer: {
                  link(_, title, text) {
                        return `<a title="${title || ""}">${text}</a>`;
                  },
                  image(_, __, title) {
                        var tag = '<i>Image file';
                        if (title)
                              tag = `${tag}: ${title}`;

                        return `${tag}</i>`;
                  },
                  blockquote() {
                        return '<i>Quote:</i>&nbsp;';
                  }
            }
      });
}

export function parse(plainMarkdown) {
      defaultOptions();

      return marked.parse(plainMarkdown, {
            smartypants: true
      });
}

export function parseRestricted(plainMarkdown) {
      restrictedOptions();

      return marked.parse(plainMarkdown, {
            smartypants: true
      });
}