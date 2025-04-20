import * as Recover from '../../output/Client.Recover.Main/index.js'
import '../../file/default/theme-switcher.js';

window.Recover = Recover;
//needed for recaptcha
window.completeRecover = function(cpt){return window.Recover.completeRecover(cpt)();}
window.Recover.main();
