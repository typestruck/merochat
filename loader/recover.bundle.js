window.Recover = require('../output/Client.Recover.Main');
//needed for recaptcha
window.completeRecover = function(cpt){return Recover.completeRecover(cpt)();}
Recover.main();
