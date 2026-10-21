function room() {
      'background only';
      let host = typeof NativeModules === 'undefined' ? undefined : NativeModules.MeroChatRoom;

      if (!host)
            throw new Error('Room storage requires the MeroChatRoom native module');

      return host;
}

function request(operation) {
      return new Promise((resolve, reject) => {
            operation((error, value) => {
                  if (error)
                        reject(new Error(error));
                  else
                        resolve(value);
            });
      });
}

export function insert_(entity, json) {
      return request(callback => room().insert(entity, json, callback));
}

export function select_(entity) {
      return request(callback => room().select(entity, callback));
}
