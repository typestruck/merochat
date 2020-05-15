exports.innerHTML_ = function(element, html) {
        element.innerHTML = html;
}

exports.innerText_ = function(element) {
        return element.innerText;
}

exports.createCustomEvent_ = function(name, stringDetail) {
        return new CustomEvent(name, {
                detail: {
                        value: stringDetail
                }
        });
}

exports.customEventDetail_ = function(event) {
        return event.detail.value;
}

exports.documentHasFocus = function() {
        return document.hasFocus();
}