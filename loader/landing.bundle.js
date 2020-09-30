window.Landing = require('../output/Client.Landing.Main');
//needed for recaptcha
window.completeRegistration = function(cpt){return Landing.completeRegistration(cpt)();}
Landing.main();

if (module.hot) {
        module.hot.accept();
}
