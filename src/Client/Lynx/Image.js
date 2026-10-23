export function pick_(onError, onSuccess) {
      'background only';

      let active = true;
      let host = typeof NativeModules === 'undefined' ? undefined : NativeModules.MeroChatImage;

      function finish(error, value) {
            if (!active)
                  return;

            active = false;

            if (error)
                  onError(error instanceof Error ? error : new Error(error));
            else
                  onSuccess(value);
      }

      if (!host)
            finish(new Error('Image selection requires the MeroChatImage native module'));
      else {
            try {
                  host.pick(finish);
            } catch (error) {
                  finish(error);
            }
      }

      return function(cancelError, onCancelerError, onCancelerSuccess) {
            active = false;
            onCancelerSuccess();
      };
}
