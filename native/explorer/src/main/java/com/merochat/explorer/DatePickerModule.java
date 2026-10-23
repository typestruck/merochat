package com.merochat.explorer;

import android.app.Activity;
import android.app.DatePickerDialog;
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
import java.util.Calendar;
import java.util.Locale;

public final class DatePickerModule extends LynxContextModule {
  private final LynxContext context;

  public DatePickerModule(LynxContext context) {
    super(context);
    this.context = context;
  }

  @LynxMethod
  public void pick(String initial, Callback callback) {
    Context current = context;

    while (current instanceof ContextWrapper && !(current instanceof Activity)) {
      current = ((ContextWrapper) current).getBaseContext();
    }
    if (!(current instanceof FragmentActivity)) {
      callback.invoke("The date picker requires an active screen", null);

      return;
    }
    FragmentActivity activity = (FragmentActivity) current;

    activity.runOnUiThread(() -> {
      if (activity.isFinishing() || activity.getSupportFragmentManager().isStateSaved()) {
        callback.invoke("The date picker cannot open while the screen is closing", null);

        return;
      }
      if (activity.getSupportFragmentManager().findFragmentByTag("merochat-date-picker") != null) {
        callback.invoke("The date picker is already open", null);

        return;
      }
      PickerFragment picker = new PickerFragment();
      Bundle arguments = new Bundle();

      arguments.putString("initial", initial);
      picker.setArguments(arguments);
      picker.callback = callback;
      try {
        picker.showNow(activity.getSupportFragmentManager(), "merochat-date-picker");
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
      Calendar date = Calendar.getInstance();
      String initial = requireArguments().getString("initial", "");

      if (!initial.isEmpty()) {
        String[] parts = initial.split("-");

        date.setLenient(false);
        date.set(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]) - 1, Integer.parseInt(parts[2]));
        date.getTimeInMillis();
      }

      return new DatePickerDialog(requireActivity(), (view, year, month, day) ->
          finish(null, String.format(Locale.ROOT, "%04d-%02d-%02d", year, month + 1, day)),
          date.get(Calendar.YEAR), date.get(Calendar.MONTH), date.get(Calendar.DAY_OF_MONTH));
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
