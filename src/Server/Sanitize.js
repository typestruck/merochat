import insane from 'insane';

export function sanitize(raw) {
      return insane(raw, { allowedTags: ["br"] });
};