export function thousands(number) {
    return number.toLocaleString('en-US', {
        maximumFractionDigits: 1,
        notation: 'compact',
        compactDisplay: 'short'
  });
}
