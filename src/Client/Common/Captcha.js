export function grecaptchaExecute() {
      grecaptcha.execute()
}

export function grecaptchaReset() {
      if (typeof grecaptcha != undefined)
            grecaptcha.reset();
}