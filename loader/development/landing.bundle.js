import * as Landing from '../../output/Client.Landing.Main/index.js';
import '../../file/default/theme-switcher.js';

//needed for recaptcha
window.initCaptchas = function () {
    Landing.initCaptchas();
};

Landing.main();