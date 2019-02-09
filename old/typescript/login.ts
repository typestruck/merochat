import * as melan from './melanchat'

document.querySelector('#login').addEventListener('click', _ => {
    melan.registerOrLogin('/login');
});