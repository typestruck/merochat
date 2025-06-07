export async function push_(url, topic, title, userId) {
    await fetch(url, {
        method: 'POST',
        body: JSON.stringify({
            'topic': topic,
            'message': '',
            'title': title,
            'click': userId + ''
        })
    });
}