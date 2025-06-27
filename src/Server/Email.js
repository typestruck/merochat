export async function sendEmail_(url, payload) {
    await fetch(url, {
        method: 'POST',
        body: JSON.stringify(payload)
    });
}