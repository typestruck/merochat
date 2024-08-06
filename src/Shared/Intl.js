let kmbFormater = new Intl.NumberFormat('en', {
    maximumFractionDigits: 1,
    notation: 'compact'
});

export function thousands(number) {
    return kmbFormater.format(number);
}
