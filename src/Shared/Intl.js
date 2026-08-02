let kmbFormater = typeof Intl === 'undefined' ? { format(number) { return number +''; } } :  new Intl.NumberFormat('en', {
    maximumFractionDigits: 1,
    notation: 'compact',
    roundingMode: 'floor'
});

export function thousands(number) {
    return kmbFormater.format(number);
}
