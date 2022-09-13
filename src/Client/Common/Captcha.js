export function execute_(id) {
      grecaptcha.execute(id)
}

export function reset_(id) {
      grecaptcha.reset(id);
}

export function render_(container, parameters, inherit) {
      let cloned = {};

      for (let key in parameters)
            cloned[key] = parameters[key];

      cloned.callback = function (response) {
            parameters.callback(response)();
      }

      grecaptcha.render(container, cloned, inherit);
}