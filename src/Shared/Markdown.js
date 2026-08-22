import { marked } from 'marked';

let maxWidth = typeof document == "undefined" ? undefined :
      (window.matchMedia('(max-width:1279px)').matches ?
            parseInt(getComputedStyle(document.querySelector('#im')).width) - 20 :
            50 * 16);

export function lexer(value) {
      return marked.lexer(value);
}

//we need all this because flame expects a custom event detail to be a json string
if (typeof window !== "undefined") {
      window.___raiseCustomEvent = function (name, w) {
            document.dispatchEvent(new CustomEvent(name, { detail: `${JSON.stringify(w)}` }));
      };
}

function defaultOptions(wrapper) {
      marked.use({
            renderer: {
                  link(href, title, text) {
                        if (!href.includes('://'))
                              href = 'http://' + href;

                        return `<a href="${href}" title="${title || ""}" target="blank">${text}</a>`;
                  },
                  image(whSrc, title, text) {
                        let result = /\[(.+)\,(.+)\](.+)/.exec(whSrc),
                              [width, height] = widthHeight(result[1], result[2]),
                              tag = `<img width="${width}" height="${height}" src="${result[3]}" alt="${title || ""}" />`

                        if (text)
                              tag += `<br/>${text}`;

                        return tag;
                  },
                  blockquote(q) {
                        let parsed = /\[[A-Za-z](\d+)\]\s(.+)/.exec(q);

                        if (parsed?.length === 3) {
                              let id = parseInt(parsed[1]),
                                    text = parsed[2];

                              return `<blockquote onclick='___raiseCustomEvent("ToQuote", ${JSON.stringify(wrapper(id))})'>${text}</blockquote>`;
                        }

                        return `<blockquote>${q}</blockquote>`;
                  },
                  html(token) {
                        return token;
                  }
            }
      });
}

function widthHeight(w, h) {
      if (maxWidth === undefined)
            return [w, h];

      let width = parseInt(w),
            height = parseInt(h);

      if (width > maxWidth)
            return [maxWidth, Math.round((height * maxWidth) / width)];

      return [width, height];
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
      return function (wrapper) {
            defaultOptions(wrapper);

            return marked.parse(plainMarkdown, {
                  gfm: true,
                  breaks: true
            });
      };
}

export function parseRestricted(plainMarkdown) {
      restrictedOptions();

      return marked.parse(plainMarkdown, {
            gfm: true,
            breaks: true
      });
}