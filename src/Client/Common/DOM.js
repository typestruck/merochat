exports.innerHTML_ = function(element, html) {
      element.innerHTML = html;
}

exports.innerText_ = function(element) {
      return element.innerText;
}

exports.createCustomEvent_ = function(name, value) {
      return new CustomEvent(name, {
            detail: {
                  value: value
            }
      });
}

exports.customEventDetail_ = function(event) {
      return event.detail.value;
}

exports.documentHasFocus = function() {
      return document.hasFocus();
}

exports.value_ = function(element) {
      return element.value || element.innerText;
}

exports.setValue_ = function(element, value) {
      if (element.value)
            element.value = value;
      else
            element.innerText = value;
}

exports.toggleDisabled_ = function(element) {
      element.disabled = !element.disabled;
}

exports.screenWidth = function() {
      return screen.width;
}

exports.requestNotificationPermission = function () {
      Notification.requestPermission();
}

exports.notificationPermission = function() {
      return Notification.permission;
}

exports.pushState_ = function(location) {
      history.pushState(null, '', location);
}

exports.scrollIntoView_ = function(element) {
      element.scrollIntoView(true);
}