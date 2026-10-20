package com.merochat.explorer;

import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.react.bridge.JavaOnlyArray;
import com.lynx.react.bridge.JavaOnlyMap;
import com.lynx.tasm.behavior.LynxContext;

/** Exposes authentication state, never cookie values. */
public final class SessionModule extends LynxContextModule {
  private final SessionStore store;
  private final LynxContext context;
  private volatile boolean destroyed;
  private final Runnable changed = this::notifyChanged;

  private void notifyChanged() {
    if (destroyed) return;
    JavaOnlyMap state = new JavaOnlyMap();
    state.putBoolean("authenticated", isAuthenticated());
    JavaOnlyArray args = new JavaOnlyArray();
    args.pushMap(state);
    context.sendGlobalEvent("merochatSessionChanged", args);
  }

  public SessionModule(LynxContext context) {
    super(context);
    this.context = context;
    store = SessionStore.get(context);
    store.addListener(changed);
  }

  @LynxMethod public void configure(String origin) { store.configure(origin); }
  @LynxMethod public boolean isAuthenticated() { return store.authenticated(); }
  @LynxMethod public void clear() { store.clear(store.generation()); }

  @Override public void destroy() {
    destroyed = true;
    store.removeListener(changed);
    super.destroy();
  }
}
