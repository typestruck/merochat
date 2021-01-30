let threeK;
let placeholderCount = 0;

exports.generate_ = function (use3000, what, size) {
      if (use3000) {
            if (threeK === undefined)
                  threeK = require('../../3000.node');

            return threeK.generate(what, size);
      }

      return 'Placeholder ' + placeholderCount++;
};