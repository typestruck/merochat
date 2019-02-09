import * as melan from './melanchat'

let url,
    uuid = (new URLSearchParams(document.location.search.substring(1))).get('u');

if (uuid) {
    url = '/recover/reset';
    (document.querySelector('#recover-email') as HTMLInputElement).style.display = 'none';
    (document.querySelector('#recover-password') as HTMLInputElement).style.display = 'block';
}
else {
    url = '/recover';
    (document.querySelector('#recover-email') as HTMLInputElement).style.display = 'block';
    (document.querySelector('#recover-password') as HTMLInputElement).style.display = 'none';
}

document.querySelector('#recover').addEventListener('click', _ => {
    let email = document.querySelector('#email') as HTMLInputElement,
        password = (document.querySelector('#password') as HTMLInputElement).value,
        confirmPassword = (document.querySelector('#confirm-password') as HTMLInputElement).value;

    if (!uuid && !email.value)
        alert('Email is required');
    else if (uuid && (!password || password !== confirmPassword))
        alert('Password and confirmation are required and must match');
    else
        melan.post(url, { email: email.value, uuid: uuid, password: password }, s => {
            if (uuid) {
                alert('Password reseted.');
                location.href = '/login';
            }
            else {                
                alert(`A recover link was sent to ${email.value}.`);
                email.value = '';
            }
        });
});