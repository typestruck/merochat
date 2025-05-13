import { marked } from 'marked';

export function lexer(value) {
      return marked.lexer(value);
}

function defaultOptions() {
      marked.use({
            renderer: {
                  link(href, title, text) {
                        if (!href.includes('://'))
                              href = 'http://' + href;

                        return `<a href="${href}" title="${title || ""}" target="blank">${text}</a>`;
                  },
                  image(whSrc, title, text) {
                        let result = /\[(.+)\,(.+)\](.+)/.exec(whSrc),
                              tag = `<img width="${result[1]}" height="${result[2]}" src="${result[3]}" alt="${title || ""}" />`

                        if (text)
                              tag += `<br/>${text}`;

                        return tag;
                  },
                  blockquote(q) {
                        return `<blockquote>${q}</blockquote>`;
                  },
                  html(token) {
                        return token;
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
                        let tag = '<i>Image file';
                        if (title)
                              tag = `${tag}: ${title}`;

                        return `${tag}</i>`;
                  },
                  blockquote() {
                        return '<i>Quote:</i>&nbsp;';
                  },
                  html(token) {
                        if (token.startsWith("<audio")) {
                              return '<i>Audio</i>&nbsp;';
                        }
                        return token;
                  }
            }
      });
}

export function parse(plainMarkdown) {
      defaultOptions();

      return marked.parse(plainMarkdown, {
            gfm: true,
            breaks: true
      });
}

export function parseRestricted(plainMarkdown) {
      restrictedOptions();

      return marked.parse(plainMarkdown, {
            gfm: true,
            breaks: true
      });
}