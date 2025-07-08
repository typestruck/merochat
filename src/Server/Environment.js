if (!process.env.PRODUCTION) {
    process.env.PORT = 8000;
    process.env.CAPTCHA_SECRET = '1d4d3315-eda9-423d-90e5-212ea4da5322';
    process.env.TOKEN_SECRET = 'so nice, so nice, I got you';
    process.env.SALT = 'put it back together';
    process.env.ADMIN_SECRET = 'HEY YOU';
}

export let port = parseInt(process.env.PORT);
export let captchaSecret = process.env.CAPTCHA_SECRET;
export let tokenSecret = process.env.TOKEN_SECRET;
export let salt = process.env.SALT;
export let adminSecret = process.env.ADMIN_SECRET;
export let databaseHost_ = process.env.DATABASE_HOST;

