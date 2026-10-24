package com.merochat.explorer;

import com.lynx.tasm.behavior.Behavior;
import com.lynx.tasm.behavior.LynxContext;
import com.lynx.tasm.behavior.LynxProp;
import com.lynx.tasm.behavior.LynxPropsHolder;
import com.lynx.tasm.behavior.ui.LynxUI;
import com.lynx.xelement.svg.LynxUISVG;
import java.util.Locale;

@LynxPropsHolder
public final class ThemedSvg extends LynxUISVG {
  public ThemedSvg(LynxContext context) {
    super(context);
  }

  @LynxProp(name = "color", defaultInt = 0xff000000)
  public void setCssColor(int color) {
    setColor(String.format(Locale.ROOT, "#%06X", color & 0xffffff));
  }

  @Override
  public void onNodeReady() {
    if (getWidth() > 0 && getHeight() > 0) {
      super.onNodeReady();
    }
  }

  public static Behavior behavior() {
    return new Behavior("svg") {
      @Override
      public LynxUI createUI(LynxContext context) {
        return new ThemedSvg(context);
      }
    };
  }
}
