export function initialize_(eventName) {
    return function () {
        'background only';

        let emitter = lynx.getJSModule('GlobalEventEmitter');
//this shit while we think better how to have multiple args in flame
        emitter.addListener('keyboardstatuschanged', (status, height) => emitter.trigger(eventName, { status, height }));
    };
}
