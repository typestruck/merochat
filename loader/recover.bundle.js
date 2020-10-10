window.Recover = require('../output/Client.Recover.Main');
//needed for recaptcha
window.completeRegistration = function(cpt){return Recover.completeRegistration(cpt)();}
Recover.main();

if (module.hot) {
        module.hot.accept();
}
