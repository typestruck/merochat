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

export function start_() {
    navigator.mediaDevices.getUserMedia(constraints).then(st, e => console.log(e));
}

export function stop_() {
    mediaRecorder.stop();

    let blob = new Blob(chunks, { type: mediaRecorder.mimeType });

    chunks = [];
    mediaRecorder = undefined;

    return blob;
}