export async function push_(url, topic, title, action) {
    await fetch(url, {
        method: 'POST',
        body: JSON.stringify({
            'topic': topic,
            'message': '',
            'title': title,
            'tlick': action
        })
    });
}