var localDateFormat = new Intl.DateTimeFormat([], { dateStyle: 'short' }),
    localWeekDayFormat = new Intl.DateTimeFormat([], { weekday: 'short' }),
    localTimeFormat = new Intl.DateTimeFormat([], { timeStyle: 'short', hour12: false });

export function time(ms) {
    return localTimeFormat.format(new Date(ms));
}

export function dayOfTheWeek(ms) {
    return localWeekDayFormat.format(new Date(ms));
}

export function fullDate(ms) {
    var dateTime = new Date(ms);

    return localDateFormat.format(dateTime);
}