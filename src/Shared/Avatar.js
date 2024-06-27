export function createImg() {
    let i = new Image();

    i.setAttribute('async', '');
    i.setAttribute('loading', 'defer');

    return i;
}