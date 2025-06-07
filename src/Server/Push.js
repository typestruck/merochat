export async function push_(url, topic, title, userId) {
    console.log('j', JSON.stringify({
        'topic': topic,
        'message': '',
        'title': title,
        'click': userId
    }));
    let r = await fetch(url, {
        method: 'POST',
        body: JSON.stringify({
            'topic': topic,
            'message': '',
            'title': title,
            'click': userId + ''
        })
    });

    let c = await r.json();
    console.log('response', c);
}