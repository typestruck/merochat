export function pick_(title) {
      return function(initial) {
            return function(options) {
                  return function(theme) {
                        return function(onError, onSuccess) {
                              'background only';

                              let active = true;
                              let host = typeof NativeModules === 'undefined' ? undefined : NativeModules.MeroChatChoicePicker;

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
                                    finish(new Error('Choice selection requires the MeroChatChoicePicker native module'));
                              else {
                                    try {
                                          host.pick(title, initial, options, theme, finish);
                                    } catch (error) {
                                          finish(error);
                                    }
                              }

                              return function(cancelError, onCancelerError, onCancelerSuccess) {
                                    active = false;
                                    onCancelerSuccess();
                              };
                        };
                  };
            };
      };
}
