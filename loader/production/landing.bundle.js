import * as Landing from '../../output-es/Client.Landing.Main/index.js'

window.Landing = Landing;
//needed for recaptcha
window.completeRegistration = function (cpt) {
      return window.Landing.completeRegistration(cpt)();
};
window.Landing.main();