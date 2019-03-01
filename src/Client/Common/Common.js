exports.localStorageSetItem = function (key, value) {
	localStorage.setItem(key, value)
}

exports.localStorageGetItem = function (key) {
	return localStorage.getItem(key) || ''
}