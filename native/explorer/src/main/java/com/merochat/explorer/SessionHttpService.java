package com.merochat.explorer;

import android.content.Context;
import com.lynx.jsbridge.network.HttpRequest;
import com.lynx.jsbridge.network.HttpResponse;
import com.lynx.jsbridge.network.HttpStreamingDelegate;
import com.lynx.react.bridge.JavaOnlyMap;
import com.lynx.tasm.service.ILynxHttpService;
import com.lynx.tasm.service.LynxHttpRequestCallback;
import java.io.IOException;
import java.util.Map;
import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public final class SessionHttpService implements ILynxHttpService {
  private final SessionStore store;
  private final OkHttpClient client = new OkHttpClient();

  public SessionHttpService(Context context) { store = SessionStore.get(context); }

  @Override public void request(HttpRequest request, LynxHttpRequestCallback callback) {
    send(request, callback, null);
  }
  @Override public void requestStreaming(HttpRequest request, LynxHttpRequestCallback callback,
      HttpStreamingDelegate delegate) {
    send(request, callback, delegate);
  }

  private void send(HttpRequest input, LynxHttpRequestCallback callback, HttpStreamingDelegate delegate) {
    HttpUrl url = HttpUrl.get(input.getUrl());
    boolean owned = store.owns(url);
    boolean authenticated = owned && store.authenticated();
    long generation = store.generation();
    boolean logout = owned && input.getHttpMethod().equalsIgnoreCase("POST") &&
        url.encodedPath().replaceAll("/+$", "").equals("/logout");
    Request.Builder builder = new Request.Builder().url(url);
    for (Map.Entry<String, Object> header : input.getHttpHeaders().asHashMap().entrySet()) {
      if (!header.getKey().equalsIgnoreCase("Cookie")) builder.header(header.getKey(), String.valueOf(header.getValue()));
    }
    String method = input.getHttpMethod();
    byte[] body = input.getHttpBody() == null ? new byte[0] : input.getHttpBody();
    builder.method(method, method.equalsIgnoreCase("GET") || method.equalsIgnoreCase("HEAD")
        ? null : RequestBody.create(null, body));
    OkHttpClient sessionClient = client.newBuilder().cookieJar(store.jar(generation)).build();
    sessionClient.newCall(builder.build()).enqueue(new Callback() {
      @Override public void onFailure(Call call, IOException error) {
        if (logout) store.clear(generation);
        HttpResponse result = new HttpResponse();
        result.setUrl(input.getUrl());
        result.setStatusCode(499);
        result.setStatusText(error.toString());
        callback.invoke(result);
        if (delegate != null) delegate.onError(error.toString());
      }
      @Override public void onResponse(Call call, Response response) throws IOException {
        try (Response closed = response) {
          if (logout || (authenticated && response.code() == 401)) store.clear(generation);
          JavaOnlyMap headers = new JavaOnlyMap();
          for (Map.Entry<String, java.util.List<String>> header : response.headers().toMultimap().entrySet()) {
            // HttpOnly cookies stay entirely on the native side.
            if (!header.getKey().equalsIgnoreCase("Set-Cookie")) headers.putString(header.getKey(), String.join(", ", header.getValue()));
          }
          HttpResponse result = new HttpResponse();
          result.setUrl(response.request().url().toString());
          result.setStatusCode(response.code());
          result.setStatusText(response.message());
          result.setHttpHeaders(headers);
          if (delegate == null) result.setHttpBody(response.body() == null ? new byte[0] : response.body().bytes());
          callback.invoke(result);
          if (delegate != null) {
            if (response.body() != null) delegate.streamingBody(response.body().byteStream());
            delegate.onEnd();
          }
        } catch (IOException error) {
          if (delegate == null) onFailure(call, error);
          else delegate.onError(error.toString());
        }
      }
    });
  }
}
