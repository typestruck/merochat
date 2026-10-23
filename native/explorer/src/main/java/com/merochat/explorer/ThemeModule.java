package com.merochat.explorer;

import android.content.res.Configuration;
import android.content.res.Resources;
import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.tasm.behavior.LynxContext;

public final class ThemeModule extends LynxContextModule {
  public ThemeModule(LynxContext context) {
    super(context);
  }

  @LynxMethod
  public String systemTheme() {
    int mode = Resources.getSystem().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;

    return mode == Configuration.UI_MODE_NIGHT_YES ? "dark" : "light";
  }
}
