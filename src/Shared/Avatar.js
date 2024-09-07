export function createImg() {
    let i = new Image();

    i.setAttribute('async', '');
    i.setAttribute('loading', 'defer');

    return i;
}

export function resetImg(node) {
    return function(a) {
        return function(b) {
            if (a != b)
                node.src = null;
            return node;
        }
    }
}
