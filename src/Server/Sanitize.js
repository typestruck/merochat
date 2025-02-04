// adapted from https://github.com/apostrophecms/sanitize-html

import * as htmlparser from 'htmlparser2';

// Tags that can conceivably represent stand-alone media.
const mediaTags = [
      'img', 'audio', 'video', 'picture', 'svg',
      'object', 'map', 'iframe', 'embed'
];

function each(obj, cb) {
      if (obj) {
            Object.keys(obj).forEach(function (key) {
                  cb(obj[key], key);
            });
      }
}

// Avoid false positives with .__proto__, .hasOwnProperty, etc.
function has(obj, key) {
      return ({}).hasOwnProperty.call(obj, key);
}

// A valid attribute name.
// We use a tolerant definition based on the set of strings defined by
// html.spec.whatwg.org/multipage/parsing.html#before-attribute-name-state
// and html.spec.whatwg.org/multipage/parsing.html#attribute-name-state .
// The characters accepted are ones which can be appended to the attribute
// name buffer without triggering a parse error:
//   * unexpected-equals-sign-before-attribute-name
//   * unexpected-null-character
//   * unexpected-character-in-attribute-name
// We exclude the empty string because it's impossible to get to the after
// attribute name state with an empty attribute name buffer.
const VALID_HTML_ATTRIBUTE_NAME = /^[^\0\t\n\f\r /<=>]+$/;

// Ignore the _recursing flag; it's there for recursive
// invocation as a guard against this exploit:
// https://github.com/fb55/htmlparser2/issues/105

function sanitizeHtml(html, options, _recursing) {
      let result = '';
      // Used for hot swapping the result variable with an empty string in order to "capture" the text written to it.
      let tempResult = '';

      function Frame(tag, attribs) {
            const that = this;
            this.tag = tag;
            this.attribs = attribs || {};
            this.tagPosition = result.length;
            this.text = ''; // Node inner text
            this.mediaChildren = [];

            this.updateParentNodeText = function () {
                  if (stack.length) {
                        const parentFrame = stack[stack.length - 1];
                        parentFrame.text += that.text;
                  }
            };

            this.updateParentNodeMediaChildren = function () {
                  if (stack.length && mediaTags.includes(this.tag)) {
                        const parentFrame = stack[stack.length - 1];
                        parentFrame.mediaChildren.push(this.tag);
                  }
            };
      }

      options = Object.assign({}, defaults, options);
      options.parser = Object.assign({}, htmlParserDefaults, options.parser);

      // Tags that contain something other than HTML, or where discarding
      // the text when the tag is disallowed makes sense for other reasons.
      // If we are not allowing these tags, we should drop their content too.
      // For other tags you would drop the tag but keep its content.
      const nonTextTagsArray = options.nonTextTags || [
            'script',
            'style',
            'textarea',
            'option'
      ];
      let allowedAttributesMap;
      let allowedAttributesGlobMap;
      if (options.allowedAttributes) {
        allowedAttributesMap = {};
        allowedAttributesGlobMap = {};
        each(options.allowedAttributes, function(attributes, tag) {
          allowedAttributesMap[tag] = [];
          const globRegex = [];
          attributes.forEach(function(obj) {
            if (typeof obj === 'string' && obj.indexOf('*') >= 0) {
              globRegex.push(escapeStringRegexp(obj).replace(/\\\*/g, '.*'));
            } else {
              allowedAttributesMap[tag].push(obj);
            }
          });
          if (globRegex.length) {
            allowedAttributesGlobMap[tag] = new RegExp('^(' + globRegex.join('|') + ')$');
          }
        });
      }

      const transformTagsMap = {};
      let depth;
      let stack;
      let skipMap;
      let transformMap;
      let skipText;
      let skipTextDepth;
      let addedText = false;

      initializeState();

      const parser = new htmlparser.Parser({
            onopentag: function (name, attribs) {
                  if (skipText) {
                        skipTextDepth++;
                        return;
                  }
                  const frame = new Frame(name, attribs);
                  stack.push(frame);

                  let skip = false;
                  const hasText = !!frame.text;
                  let transformedTag;
                  if (has(transformTagsMap, name)) {
                        transformedTag = transformTagsMap[name](name, attribs);

                        frame.attribs = attribs = transformedTag.attribs;

                        if (transformedTag.text !== undefined) {
                              frame.innerText = transformedTag.text;
                        }

                        if (name !== transformedTag.tagName) {
                              frame.name = name = transformedTag.tagName;
                              transformMap[depth] = transformedTag.tagName;
                        }
                  }

                  if (!options.allowedTags.includes(name)) {
                        skip = true;
                        skipMap[depth] = true;

                        if (nonTextTagsArray.includes(name)) {
                              skipText = true;
                              skipTextDepth = 1;
                        }

                        skipMap[depth] = true;
                  }
                  depth++;
                  if (skip) {

                        // We want the contents but not this tag
                        if (frame.innerText && !hasText) {
                              const escaped = escapeHtml(frame.innerText);
                              if (options.textFilter) {
                                    result += options.textFilter(escaped, name);
                              } else {
                                    result += escapeHtml(frame.innerText);
                              }
                              addedText = true;
                        }
                        return;


                  }
                  result += '<' + name;

                  if (!allowedAttributesMap || has(allowedAttributesMap, name) || allowedAttributesMap['*']) {
                        each(attribs, function (value, a) {
                              if (!VALID_HTML_ATTRIBUTE_NAME.test(a)) {
                                    // This prevents part of an attribute name in the output from being
                                    // interpreted as the end of an attribute, or end of a tag.
                                    delete frame.attribs[a];
                                    return;
                              }

                              // check allowedAttributesMap for the element and attribute and modify the value
                              // as necessary if there are specific values defined.
                              let passedAllowedAttributesMapCheck = false;
                              if (!allowedAttributesMap ||
                                    (has(allowedAttributesMap, name) && allowedAttributesMap[name].indexOf(a) !== -1) ||
                                    (allowedAttributesMap['*'] && allowedAttributesMap['*'].indexOf(a) !== -1) ||
                                    (has(allowedAttributesGlobMap, name) && allowedAttributesGlobMap[name].test(a)) ||
                                    (allowedAttributesGlobMap['*'] && allowedAttributesGlobMap['*'].test(a))) {
                                    passedAllowedAttributesMapCheck = true;
                              } else if (allowedAttributesMap && allowedAttributesMap[name]) {
                                    for (const o of allowedAttributesMap[name]) {
                                          if (o.name && (o.name === a)) {
                                                passedAllowedAttributesMapCheck = true;
                                                let newValue = '';
                                                if (o.multiple === true) {
                                                      // verify the values that are allowed
                                                      const splitStrArray = value.split(' ');
                                                      for (const s of splitStrArray) {
                                                            if (o.values.indexOf(s) !== -1) {
                                                                  if (newValue === '') {
                                                                        newValue = s;
                                                                  } else {
                                                                        newValue += ' ' + s;
                                                                  }
                                                            }
                                                      }
                                                } else if (o.values.indexOf(value) >= 0) {
                                                      // verified an allowed value matches the entire attribute value
                                                      newValue = value;
                                                }
                                                value = newValue;
                                          }
                                    }
                              }
                              if (passedAllowedAttributesMapCheck) {
                                    if (options.allowedSchemesAppliedToAttributes.indexOf(a) !== -1) {
                                          if (naughtyHref(name, value)) {
                                                delete frame.attribs[a];
                                                return;
                                          }
                                    }

                                    result += ' ' + a;
                                    if (value && value.length) {
                                          result += '="' + escapeHtml(value, true) + '"';
                                    } else if (options.allowedEmptyAttributes.includes(a)) {
                                          result += '=""';
                                    }
                              } else {
                                    delete frame.attribs[a];
                              }
                        });
                  }
                  if (options.selfClosing.includes(name)) {
                        result += ' />';  // Be XHTML-compliant
                  } else {
                        result += '>';
                        if (frame.innerText && !hasText && !options.textFilter) {
                              result += escapeHtml(frame.innerText);
                              addedText = true;
                        }
                  }
                  if (skip) {
                        result = tempResult + escapeHtml(result);
                        tempResult = '';
                  }
            },
            ontext: function (text) {
                  if (skipText) {
                        return;
                  }
                  const lastFrame = stack[stack.length - 1];
                  let tag;

                  if (lastFrame) {
                        tag = lastFrame.tag;
                        // If inner text was set by transform function then let's use it
                        text = lastFrame.innerText !== undefined ? lastFrame.innerText : text;
                  }


                  const escaped = escapeHtml(text, false);
                  if (options.textFilter && !addedText) {
                        result += options.textFilter(escaped, tag);
                  } else if (!addedText) {
                        result += escaped;
                  }

                  if (stack.length) {
                        const frame = stack[stack.length - 1];
                        frame.text += text;
                  }
            },
            onclosetag: function (name, isImplied) {

                  if (skipText) {
                        skipTextDepth--;
                        if (!skipTextDepth) {
                              skipText = false;
                        } else {
                              return;
                        }
                  }

                  const frame = stack.pop();
                  if (!frame) {
                        // Do not crash on bad markup
                        return;
                  }

                  if (frame.tag !== name) {
                        // Another case of bad markup.
                        // Push to stack, so that it will be used in future closing tags.
                        stack.push(frame);
                        return;
                  }

                  skipText = options.enforceHtmlBoundary ? name === 'html' : false;
                  depth--;
                  const skip = skipMap[depth];
                  if (skip) {
                        delete skipMap[depth];
                        frame.updateParentNodeText();
                        return;

                  }

                  if (transformMap[depth]) {
                        name = transformMap[depth];
                        delete transformMap[depth];
                  }



                  frame.updateParentNodeMediaChildren();
                  frame.updateParentNodeText();

                  if (
                        // Already output />
                        options.selfClosing.indexOf(name) !== -1 ||
                        // Escaped tag, closing tag is implied
                        (isImplied && !options.allowedTags.includes(name) && ['escape', 'recursiveEscape'].indexOf(options.disallowedTagsMode) >= 0)
                  ) {
                        if (skip) {
                              result = tempResult;
                              tempResult = '';
                        }
                        return;
                  }

                  result += '</' + name + '>';
                  if (skip) {
                        result = tempResult + escapeHtml(result);
                        tempResult = '';
                  }
                  addedText = false;
            }
      }, options.parser);
      parser.write(html);
      parser.end();

      return result;

      function initializeState() {
            result = '';
            depth = 0;
            stack = [];
            skipMap = {};
            transformMap = {};
            skipText = false;
            skipTextDepth = 0;
      }

      function escapeHtml(s, quote) {
            s = s.replace(/&/g, '&amp;').replace(/</g, '&lt;');
            if (quote) {
                  s = s.replace(/"/g, '&quot;');
            }

            return s;
      }

      function naughtyHref(name, href) {
            // Browsers ignore character codes of 32 (space) and below in a surprising
            // number of situations. Start reading here:
            // https://www.owasp.org/index.php/XSS_Filter_Evasion_Cheat_Sheet#Embedded_tab
            // eslint-disable-next-line no-control-regex
            href = href.replace(/[\x00-\x20]+/g, '');
            // Clobber any comments in URLs, which the browser might
            // interpret inside an XML data island, allowing
            // a javascript: URL to be snuck through
            while (true) {
                  const firstIndex = href.indexOf('<!--');
                  if (firstIndex === -1) {
                        break;
                  }
                  const lastIndex = href.indexOf('-->', firstIndex + 4);
                  if (lastIndex === -1) {
                        break;
                  }
                  href = href.substring(0, firstIndex) + href.substring(lastIndex + 3);
            }
            // Case insensitive so we don't get faked out by JAVASCRIPT #1
            // Allow more characters after the first so we don't get faked
            // out by certain schemes browsers accept
            const matches = href.match(/^([a-zA-Z][a-zA-Z0-9.\-+]*):/);
            if (!matches) {
                  // Protocol-relative URL starting with any combination of '/' and '\'
                  if (href.match(/^[/\\]{2}/)) {
                        return !options.allowProtocolRelative;
                  }

                  // No scheme
                  return false;
            }
            const scheme = matches[1].toLowerCase();

            if (has(options.allowedSchemesByTag, name)) {
                  return options.allowedSchemesByTag[name].indexOf(scheme) === -1;
            }

            return !options.allowedSchemes || options.allowedSchemes.indexOf(scheme) === -1;
      }

      function filterClasses(classes, allowed, allowedGlobs) {
            if (!allowed) {
                  // The class attribute is allowed without filtering on this tag
                  return classes;
            }
            classes = classes.split(/\s+/);
            return classes.filter(function (clss) {
                  return allowed.indexOf(clss) !== -1 || allowedGlobs.some(function (glob) {
                        return glob.test(clss);
                  });
            }).join(' ');
      }
}

// Defaults are accessible to you so that you can use them as a starting point
// programmatically if you wish

let htmlParserDefaults = {
      decodeEntities: true
};

let defaults = {
      allowedTags: [

      ],
      // Tags that cannot be boolean
      nonBooleanAttributes: [

      ],
      disallowedTagsMode: 'discard',
      allowedAttributes: {
      },
      allowedEmptyAttributes: [
      ],
      // Lots of these won't come up by default because we don't allow them
      selfClosing: [],
      // URL schemes we permit
      allowedSchemes: ['http', 'https', 'ftp', 'mailto', 'tel'],
      allowedSchemesByTag: {},
      allowedSchemesAppliedToAttributes: ['href', 'src', 'cite'],
      allowProtocolRelative: true,
      parseStyleAttributes: true
};

sanitizeHtml.simpleTransform = function (newTagName, newAttribs, merge) {
      merge = (merge === undefined) ? true : merge;
      newAttribs = newAttribs || {};

      return function (_, attribs) {
            let attrib;
            if (merge) {
                  for (attrib in newAttribs) {
                        attribs[attrib] = newAttribs[attrib];
                  }
            } else {
                  attribs = newAttribs;
            }

            return {
                  tagName: newTagName,
                  attribs: attribs
            };
      };
};

export function sanitize(raw) {
      return sanitizeHtml(raw, { allowedTags: ["br", "audio"], allowedAttributes: { audio: ["controls", "src"] } });
};