export async function push_(url, title, action) {
    let r = await fetch(url, {
        method: 'POST',
        headers: {
            'Title': title,
            'Click': action
        },
        body: ""
    });
    let a = await r.json();

    console.log('response', a);
}