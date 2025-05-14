//assume only one recording at a time
let mediaRecorder,
    chunks = [],
    lock;

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

            if (lock != undefined)
                lock.release().then(() => {
                    lock = undefined
                });
        }
    };
}

export function start_(constraints, options, handler) {
    navigator.wakeLock.request('screen').then(l => {
      lock = l;
    }).finally(() => navigator.mediaDevices.getUserMedia(constraints).then(st(options, handler), e => console.log(e)));
}

export function stop_() {
    mediaRecorder.stop();
    mediaRecorder = null;
}