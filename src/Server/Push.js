export function push_(url, title, action) {
    fetch(url, {
        method: 'POST',
        headers: {
            'Title': title,
            'Click': action
        },
        body: ""
    });
}