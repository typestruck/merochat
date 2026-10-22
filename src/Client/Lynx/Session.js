import { parse, resolve } from 'url';

let nativeSession,
    origin,
    authenticated = false;

function sessionEnded() {}

export function clear() {
    'background only';

    nativeSession.clear();
    authenticated = false;
}

export function initialize_(apiUrl, websocketUrl) {
    'background only';

    if (nativeSession)
        return;

    let api = parse(apiUrl);

    if (!/^https?:$/.test(api.protocol) || !api.host || api.auth ||
        (api.pathname && api.pathname !== '/') || api.search || api.hash) {
        throw new Error('LYNX_API_URL must be an HTTP(S) origin');
    }
    origin = `${api.protocol}//${api.host}`;

    let host = NativeModules.MeroChatSession;

    if (!host)
        throw new Error('Cookie authentication requires MeroChat Explorer');

    host.configure(origin);
    host.configureWebSocket(websocketUrl);
    nativeSession = host;
    authenticated = host.isAuthenticated();
    lynx.getJSModule('GlobalEventEmitter').addListener('merochatSessionChanged', state => {
        let ended = authenticated && !state.authenticated;

        authenticated = state.authenticated;
        if (ended)
            sessionEnded();
    });

    let nativeFetch = fetch;

    fetch = async function (input, options = {}) {
        let url = resolve(`${origin}/`, input),
            target = parse(url);

        if (`${target.protocol}//${target.host}` !== origin)
            return nativeFetch(url, options);

        let response = await nativeFetch(url, { ...options, credentials: 'include' }),
            path = target.pathname.replace(/\/+$/, '') || '/';

        if (options.method?.toUpperCase() === 'POST' && response.status >= 200 && response.status < 300 &&
            ['/login', '/register', '/temporary'].includes(path) && !isAuthenticated()) {
            throw new Error('The native host did not establish a session');
        }

        return response;
    };
}

export function onSessionEnded(handler) {
    return function () {
        'background only';

        sessionEnded = handler;
    };
}

export function isNativeSession() {
    'background only';

    return !!nativeSession;
}

export function isAuthenticated() {
    'background only';

    if (!nativeSession)
        return true;

    authenticated = nativeSession.isAuthenticated();

    return authenticated;
}
