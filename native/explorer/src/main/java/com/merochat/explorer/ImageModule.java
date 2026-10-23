package com.merochat.explorer;

import android.app.Activity;
import android.content.Context;
import android.content.ContextWrapper;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.media.ExifInterface;
import android.net.Uri;
import android.os.Bundle;
import android.util.Base64;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.react.bridge.Callback;
import com.lynx.tasm.behavior.LynxContext;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class ImageModule extends LynxContextModule {
  private static final ExecutorService executor = Executors.newSingleThreadExecutor();
  private final LynxContext context;

  public ImageModule(LynxContext context) {
    super(context);
    this.context = context;
  }

  @LynxMethod
  public void pick(Callback callback) {
    Context current = context;

    while (current instanceof ContextWrapper && !(current instanceof Activity)) {
      current = ((ContextWrapper) current).getBaseContext();
    }
    if (!(current instanceof FragmentActivity)) {
      callback.invoke("The image picker requires an active screen", null);

      return;
    }
    FragmentActivity activity = (FragmentActivity) current;

    activity.runOnUiThread(() -> {
      if (activity.isFinishing() || activity.getSupportFragmentManager().isStateSaved()) {
        callback.invoke("The image picker cannot open while the screen is closing", null);

        return;
      }
      if (activity.getSupportFragmentManager().findFragmentByTag("merochat-image-picker") != null) {
        callback.invoke("The image picker is already open", null);

        return;
      }
      PickerFragment picker = new PickerFragment();

      picker.callback = callback;
      try {
        activity.getSupportFragmentManager().beginTransaction()
            .add(picker, "merochat-image-picker").commitNow();
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);

        intent.setType("image/*");
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        picker.startActivityForResult(intent, 1);
      } catch (Exception error) {
        picker.finish(error.getMessage(), null);
      }
    });
  }

  public static final class PickerFragment extends Fragment {
    private Callback callback;

    @Override
    public void onCreate(Bundle state) {
      super.onCreate(state);
      setRetainInstance(true);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
      super.onActivityResult(requestCode, resultCode, data);
      if (requestCode != 1) return;
      if (resultCode != Activity.RESULT_OK || data == null || data.getData() == null) {
        finish(null, null);

        return;
      }
      Context application = requireContext().getApplicationContext();
      Uri uri = data.getData();

      executor.execute(() -> {
        try {
          finish(null, encode(application, uri));
        } catch (Exception error) {
          finish("Unable to read this picture: " + error.getMessage(), null);
        }
      });
    }

    private void finish(String error, String value) {
      Callback result = callback;

      callback = null;
      if (result != null) result.invoke(error, value);
      new android.os.Handler(android.os.Looper.getMainLooper()).post(() -> {
        if (isAdded()) getParentFragmentManager().beginTransaction().remove(this).commitAllowingStateLoss();
      });
    }
  }

  private static String encode(Context context, Uri uri) throws Exception {
    BitmapFactory.Options options = new BitmapFactory.Options();

    options.inJustDecodeBounds = true;
    try (InputStream input = context.getContentResolver().openInputStream(uri)) {
      BitmapFactory.decodeStream(input, null, options);
    }
    if (options.outWidth <= 0 || options.outHeight <= 0) {
      throw new IllegalArgumentException("Unsupported image format");
    }
    options.inSampleSize = 1;
    while (Math.max(options.outWidth, options.outHeight) / options.inSampleSize > 1600) {
      options.inSampleSize *= 2;
    }
    options.inJustDecodeBounds = false;
    Bitmap bitmap;

    try (InputStream input = context.getContentResolver().openInputStream(uri)) {
      bitmap = BitmapFactory.decodeStream(input, null, options);
    }
    if (bitmap == null) throw new IllegalArgumentException("Unsupported image format");
    try {
      int orientation = ExifInterface.ORIENTATION_NORMAL;

      try (InputStream input = context.getContentResolver().openInputStream(uri)) {
        orientation = new ExifInterface(input).getAttributeInt(
            ExifInterface.TAG_ORIENTATION, ExifInterface.ORIENTATION_NORMAL);
      } catch (java.io.IOException ignored) {
        orientation = ExifInterface.ORIENTATION_NORMAL;
      }
      Matrix transform = new Matrix();

      switch (orientation) {
        case ExifInterface.ORIENTATION_FLIP_HORIZONTAL: transform.setScale(-1, 1); break;
        case ExifInterface.ORIENTATION_ROTATE_180: transform.setRotate(180); break;
        case ExifInterface.ORIENTATION_FLIP_VERTICAL: transform.setScale(1, -1); break;
        case ExifInterface.ORIENTATION_TRANSPOSE: transform.setRotate(90); transform.postScale(-1, 1); break;
        case ExifInterface.ORIENTATION_ROTATE_90: transform.setRotate(90); break;
        case ExifInterface.ORIENTATION_TRANSVERSE: transform.setRotate(270); transform.postScale(-1, 1); break;
        case ExifInterface.ORIENTATION_ROTATE_270: transform.setRotate(270); break;
        default: break;
      }
      float scale = Math.min(1f, 800f / Math.max(bitmap.getWidth(), bitmap.getHeight()));

      transform.postScale(scale, scale);
      Bitmap resized = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), transform, true);

      if (resized != bitmap) {
        bitmap.recycle();
        bitmap = resized;
      }
      boolean alpha = bitmap.hasAlpha();
      ByteArrayOutputStream output = new ByteArrayOutputStream();

      if (!bitmap.compress(alpha ? Bitmap.CompressFormat.PNG : Bitmap.CompressFormat.JPEG, 94, output)) {
        throw new IllegalArgumentException("Unable to encode image");
      }

      return "data:image/" + (alpha ? "png" : "jpeg") + ";base64,"
          + Base64.encodeToString(output.toByteArray(), Base64.NO_WRAP);
    } finally {
      bitmap.recycle();
    }
  }
}
