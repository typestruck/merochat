exports.time = function (ms) {
    var dateTime = new Date(ms),
        hours = dateTime.getHours() + '',
        minutes = dateTime.getMinutes() + '';

    return hours.padStart(2, '0') +  ':' + minutes.padStart(2, '0');
}

exports.dayOfTheWeek = function (ms) {
    return new Date(ms).toString().split(' ')[0];
}

exports.fullDate = function (ms) {
    var dateTime = new Date(ms);

    return dateTime.toLocaleDateString() + ' ' + dateTime.toLocaleTimeString();
}