var localDateFormat = new Intl.DateTimeFormat([], { dateStyle: 'short' }),
    localWeekDayFormat = new Intl.DateTimeFormat([], { weekday: 'short' }),
    localTimeFormat = new Intl.DateTimeFormat([], { timeStyle: 'short', hour12: false });

exports.time = function (ms) {
    return localTimeFormat.format(new Date(ms));
}

exports.dayOfTheWeek = function (ms) {
    return localWeekDayFormat.format(new Date(ms));
}

exports.fullDate = function (ms) {
    var dateTime = new Date(ms);

    return localDateFormat.format(dateTime);
}