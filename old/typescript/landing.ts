import * as melan from "./melanchat";

//used as grecaptcha callback
window['completeSignUp'] = captchaResponse => melan.registerOrLogin('/register', false, captchaResponse);

document.querySelector('#create-account').addEventListener('click', _ => {
    melan.registerOrLogin('/register', true);
});