var marked = require('marked');
var insane = require('insane');

exports.parse = function (input) {
        //looks like you need to overwrite previous calls to marked.use if you dont want to pass an option
        var image = new marked.Renderer().image;

        marked.use({
                renderer: {
                        link(href, title, text) {
                                if (!href.includes('://'))
                                        href = 'http://' + href;

                                return `<a href="${href}" title="${title || ""}" target="blank">${text}</a>`;
                        },
                        image
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