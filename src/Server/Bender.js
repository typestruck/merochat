let bender;
let placeholderCount = 0;

exports.generate_ = function (useBender, what, size) {
      if (useBender) {
            if (bender === undefined)
                  bender = require('../../bender.node');

            return bender.generate(what, size);
      }

      return 'Placeholder ' + placeholderCount++;
};