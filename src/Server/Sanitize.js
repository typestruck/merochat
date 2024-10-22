import insane from 'insane';

export function sanitize(raw) {
      return insane(raw, { allowedTags: ["br", "audio"],    allowedAttributes: { audio: ["controls", "src"] } });
};