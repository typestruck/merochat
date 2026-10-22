import '@formatjs/intl-getcanonicallocales/polyfill.js';
import '@formatjs/intl-locale/polyfill.js';
import '@formatjs/intl-pluralrules/polyfill.js';
import '@formatjs/intl-pluralrules/locale-data/en.js';
import '@formatjs/intl-numberformat/polyfill.js';
import '@formatjs/intl-numberformat/locale-data/en.js';
import '@formatjs/intl-datetimeformat/polyfill.js';
import '@formatjs/intl-datetimeformat/locale-data/en.js';

let polyfilled = Intl.DateTimeFormat.polyfilled,
    timeZone = polyfilled ? 'UTC' : undefined,
    localDateFormat = new Intl.DateTimeFormat([], { dateStyle: 'short', hourCycle: 'h23', timeZone }),
    localWeekDayFormat = new Intl.DateTimeFormat([], { weekday: 'short', hourCycle: 'h23', timeZone }),
    localTimeFormat = new Intl.DateTimeFormat([], { hour: '2-digit', minute: '2-digit', hourCycle: 'h23', timeZone });

function formatLocal(formatter, ms) {
    let date = new Date(ms);

    return formatter.format(polyfilled ? ms - date.getTimezoneOffset() * 60000 : date);
}

export function time(ms) {

    return formatLocal(localTimeFormat, ms);
}

export function dayOfTheWeek(ms) {

    return formatLocal(localWeekDayFormat, ms);
}

export function fullDate(ms) {

    return formatLocal(localDateFormat, ms);
}
