export async function push_(url, title, action) {
    await fetch(url, {
        method: 'POST',
        headers: {
            'Title': title,
            'Click': action
        },
        body: ""
    });
}