exports.grecaptchaExecute = function () {
      grecaptcha.execute()
}

exports.grecaptchaReset = function () {
      if (typeof grecaptcha != undefined)
            grecaptcha.reset();
}