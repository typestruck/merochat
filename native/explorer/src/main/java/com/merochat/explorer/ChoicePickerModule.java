package com.merochat.explorer;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.res.Configuration;
import android.app.Dialog;
import android.content.Context;
import android.content.ContextWrapper;
import android.content.DialogInterface;
import android.os.Bundle;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.FragmentActivity;
import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.react.bridge.Callback;
import com.lynx.tasm.behavior.LynxContext;
import com.lynx.react.bridge.ReadableArray;
import com.lynx.react.bridge.ReadableMap;

public final class ChoicePickerModule extends LynxContextModule {
  private final LynxContext context;

  public ChoicePickerModule(LynxContext context) {
    super(context);
    this.context = context;
  }

  @LynxMethod
  public void pick(String title, String initial, ReadableArray options, String theme, Callback callback) {
    Context current = context;

    while (current instanceof ContextWrapper && !(current instanceof Activity)) {
      current = ((ContextWrapper) current).getBaseContext();
    }
    if (!(current instanceof FragmentActivity)) {
      callback.invoke("The choice picker requires an active screen", null);

      return;
    }
    FragmentActivity activity = (FragmentActivity) current;

    activity.runOnUiThread(() -> {
      if (activity.isFinishing() || activity.getSupportFragmentManager().isStateSaved()) {
        callback.invoke("The choice picker cannot open while the screen is closing", null);

        return;
      }
      if (activity.getSupportFragmentManager().findFragmentByTag("merochat-choice-picker") != null) {
        callback.invoke("The choice picker is already open", null);

        return;
      }
      PickerFragment picker = new PickerFragment();
      Bundle arguments = new Bundle();

      String[] labels = new String[options.size()];
      String[] values = new String[options.size()];

      for (int index = 0; index < options.size(); index++) {
        ReadableMap option = options.getMap(index);

        labels[index] = option.getString("name");
        values[index] = option.getString("value");
      }
      arguments.putString("title", title);
      arguments.putString("theme", theme);
      arguments.putStringArray("labels", labels);
      arguments.putStringArray("values", values);
      arguments.putString("initial", initial);
      picker.setArguments(arguments);
      picker.callback = callback;
      try {
        picker.showNow(activity.getSupportFragmentManager(), "merochat-choice-picker");
      } catch (Exception error) {
        picker.finish(error.getMessage(), null);
      }
    });
  }

  public static final class PickerFragment extends DialogFragment {
    private Callback callback;

    @Override
    public void onCreate(Bundle state) {
      super.onCreate(state);
      setRetainInstance(true);
    }

    @Override
    public Dialog onCreateDialog(Bundle state) {
      Bundle arguments = requireArguments();
      String[] values = arguments.getStringArray("values");
      String initial = arguments.getString("initial");
      String theme = arguments.getString("theme", "");
      boolean dark = theme.equals("dark") || (!theme.equals("light") &&
          (getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES);
      int selected = -1;

      for (int index = 0; index < values.length; index++) {
        if (values[index].equals(initial)) selected = index;
      }

      AlertDialog.Builder builder = new AlertDialog.Builder(requireActivity(), dark ? android.R.style.Theme_Material_Dialog_Alert : android.R.style.Theme_Material_Light_Dialog_Alert)
          .setTitle(arguments.getString("title"))
          .setNegativeButton(android.R.string.cancel, (dialog, which) -> dismiss());
      DialogInterface.OnClickListener choose = (dialog, index) -> {
        finish(null, values[index]);
        dismiss();
      };

      if (initial == null) {
        builder.setItems(arguments.getStringArray("labels"), choose);
      } else {
        builder.setSingleChoiceItems(arguments.getStringArray("labels"), selected, choose);
      }

      return builder.create();
    }

    @Override
    public void onDismiss(DialogInterface dialog) {
      super.onDismiss(dialog);
      finish(null, null);
    }

    private void finish(String error, String value) {
      Callback result = callback;

      callback = null;
      if (result != null) result.invoke(error, value);
    }
  }
}
