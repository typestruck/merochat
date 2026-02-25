let kmbFormater = new Intl.NumberFormat('en', {
    maximumFractionDigits: 1,
    notation: 'compact',
    roundingMode: 'floor'
});

export function thousands(number) {
    return kmbFormater.format(number);
}
