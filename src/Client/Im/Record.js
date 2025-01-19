//assume only one recording at a time
let mediaRecorder,
    chunks = [];

function st(options, handler) {
    return function (stream) {
        mediaRecorder = new MediaRecorder(stream, options);
        mediaRecorder.start();

        mediaRecorder.ondataavailable = e => {
            chunks.push(e.data);
        };
        mediaRecorder.onstop = e => {
            let reader = new FileReader();

            reader.onloadend = () => {
                handler(reader.result);
                chunks = [];
            };
            reader.readAsDataURL(new Blob(chunks, { type: mediaRecorder.mimeType }));
        }
    };
}

export function start_(constraints, options, handler) {
    navigator.mediaDevices.getUserMedia(constraints).then(st(options, handler), e => console.log(e));
}

export function stop_() {
    mediaRecorder.stop();
}