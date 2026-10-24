package com.merochat.explorer;

import android.content.Intent;
import android.net.Uri;
import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.tasm.behavior.LynxContext;

public final class LinksModule extends LynxContextModule {
  private final LynxContext context;

  public LinksModule(LynxContext context) {
    super(context);
    this.context = context;
  }

  @LynxMethod
  public void open(String url) {
    Uri uri = Uri.parse(url);

    if (uri.getScheme() == null) uri = Uri.parse("https://mero.chat").buildUpon().encodedPath(uri.getPath()).encodedQuery(uri.getEncodedQuery()).encodedFragment(uri.getEncodedFragment()).build();
    String scheme = uri.getScheme();

    if (!"https".equals(scheme) && !"http".equals(scheme) && !"mailto".equals(scheme)) return;
    Intent intent = new Intent(Intent.ACTION_VIEW, uri);

    if (!"mailto".equals(scheme)) {
      Intent browser = new Intent(Intent.ACTION_VIEW, Uri.parse("https://example.com"));

      browser.addCategory(Intent.CATEGORY_BROWSABLE);
      intent.setSelector(browser);
      intent.addCategory(Intent.CATEGORY_BROWSABLE);
    }
    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    context.startActivity(intent);
  }
}
