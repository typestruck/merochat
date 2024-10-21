//assume only one recording at a time
let chunks = [],
    mediaRecorder;

function st(stream) {
    mediaRecorder = new MediaRecorder(stream);
    mediaRecorder.start();

    mediaRecorder.ondataavailable = e => {
        chunks.push(e.data);
    };
}

export function start_(constraints) {
    navigator.mediaDevices.getUserMedia(constraints).then(st, e => console.log(e));
}

export function stop_() {
    mediaRecorder.stop();

    let base64 = 'data:audio/webm;base64,' + btoa((new Blob(chunks, { type: mediaRecorder.mimeType })).text());

    chunks = [];
    mediaRecorder = undefined;

    return base64;
}