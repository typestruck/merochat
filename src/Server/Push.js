export async function push_(url, topic, title, message) {
    await fetch(url, {
        method: 'POST',
        body: JSON.stringify({
            topic,
            title,
            message
        })
    });
}