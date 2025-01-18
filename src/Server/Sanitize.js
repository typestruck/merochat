import sanitizeHtml from 'sanitize-html';

export function sanitize(raw) {
      return sanitizeHtml(raw, { allowedTags: ["br", "audio"], allowedAttributes: { audio: ["controls", "src"] } });
};