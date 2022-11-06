var localDateFormat = new Intl.DateTimeFormat([], { dateStyle: 'short', hourCycle: "h23" }),
    localWeekDayFormat = new Intl.DateTimeFormat([], { weekday: 'short', hourCycle: "h23" }),
    localTimeFormat = new Intl.DateTimeFormat([], { timeStyle: 'short', hourCycle: "h23" });

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