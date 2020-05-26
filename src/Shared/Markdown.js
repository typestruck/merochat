var marked = require('marked');
var insane = require('insane')

exports.parse = function (input) {
        marked.use({
                renderer: {
                        link(href, title, text) {
                                return `<a href="${href}" title="${title || ""}" target="blank">${text}</a>`;
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

exports.sanitize = insane;