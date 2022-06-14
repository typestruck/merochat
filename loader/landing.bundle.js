import { main } from '../output/Client.Landing.Main/index.js'

window.Landing = main;
//needed for recaptcha
window.completeRegistration = function (cpt) {
      return Landing.completeRegistration(cpt)();
};
Landing.main();
