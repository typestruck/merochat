export async function sendEmail_(url, userId, recordId, emailOption) {
    await fetch(url, {
        method: 'POST',
        body: JSON.stringify({
            userId,
            recordId,
            emailOption
        })
    });
}