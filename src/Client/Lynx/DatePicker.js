export function pick_(initial) {

      return function(onError, onSuccess) {
            'background only';

            let active = true;
            let host = typeof NativeModules === 'undefined' ? undefined : NativeModules.MeroChatDatePicker;

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
                  finish(new Error('Date selection requires the MeroChatDatePicker native module'));
            else {
                  try {
                        host.pick(initial, finish);
                  } catch (error) {
                        finish(error);
                  }
            }

            return function(cancelError, onCancelerError, onCancelerSuccess) {
                  active = false;
                  onCancelerSuccess();
            };
      };
}
