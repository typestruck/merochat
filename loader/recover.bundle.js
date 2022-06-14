import { main } from '../output/Client.Recover.Main/index.js'

window.Recover = main;
//needed for recaptcha
window.completeRecover = function(cpt){return Recover.completeRecover(cpt)();}
Recover.main();
