package com.merochat.explorer;

import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.react.bridge.JavaOnlyArray;
import com.lynx.react.bridge.JavaOnlyMap;
import com.lynx.react.bridge.ReadableArray;
import com.lynx.react.bridge.ReadableMap;
import com.lynx.tasm.behavior.LynxContext;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.WebSocket;
import okhttp3.WebSocketListener;

public final class SessionWebSocketModule extends LynxContextModule {
  private final SessionStore store;
  private final Map<Integer, WebSocket> sockets = new ConcurrentHashMap<>();
  private final OkHttpClient client = new OkHttpClient.Builder()
      .readTimeout(0, TimeUnit.MILLISECONDS).followRedirects(false).followSslRedirects(false).build();
  private final Runnable changed = this::sessionChanged;
  private long generation;
  private volatile boolean destroyed;

  public SessionWebSocketModule(LynxContext context) {
    super(context);
    store = SessionStore.get(context);
    generation = store.generation();
    store.addListener(changed);
  }

  private void sessionChanged() {
    if (generation != store.generation() || !store.authenticated()) {
      for (WebSocket socket : sockets.values()) socket.close(1000, "Session ended");
      sockets.clear();
      generation = store.generation();
    }
  }

  private JavaOnlyMap event(int id) {
    JavaOnlyMap data = new JavaOnlyMap();

    data.putInt("id", id);

    return data;
  }

  private void emit(String name, JavaOnlyMap data) {
    if (destroyed) return;

    JavaOnlyArray args = new JavaOnlyArray();

    args.pushMap(data);
    mLynxContext.sendGlobalEvent(name, args);
  }

  private void failed(int id, String message) {
    JavaOnlyMap data = event(id);

    data.putString("message", message);
    emit("websocketFailed", data);
  }

  @LynxMethod
  public void connect(String url, ReadableArray protocols, ReadableMap options, double socketID) {
    int id = (int) socketID;

    try {
      sessionChanged();
      if (!store.authenticated()) throw new IllegalStateException("Session is not authenticated");

      Request.Builder request = new Request.Builder().url(url);
      String cookies = store.websocketCookies(request.build().url());
      long connectedGeneration = store.generation();
      StringBuilder protocolHeader = new StringBuilder();

      if (protocols != null) {
        for (int i = 0; i < protocols.size(); i++) {
          if (i > 0) protocolHeader.append(", ");
          protocolHeader.append(protocols.getString(i));
        }
      }
      if (protocolHeader.length() > 0) request.header("Sec-WebSocket-Protocol", protocolHeader.toString());
      request.header("Cookie", cookies);
      WebSocket socket = client.newWebSocket(request.build(), new WebSocketListener() {
        @Override public void onOpen(WebSocket socket, Response response) {
          if (destroyed || connectedGeneration != store.generation()) {
            socket.close(1000, "Session ended");
            return;
          }

          JavaOnlyMap data = event(id);

          data.putString("protocol", response.header("Sec-WebSocket-Protocol", ""));
          emit("websocketOpen", data);
        }

        @Override public void onMessage(WebSocket socket, String message) {
          JavaOnlyMap data = event(id);

          data.putString("type", "text");
          data.putString("data", message);
          emit("websocketMessage", data);
        }

        @Override public void onClosing(WebSocket socket, int code, String reason) {
          socket.close(code, reason);
        }

        @Override public void onClosed(WebSocket socket, int code, String reason) {
          sockets.remove(id, socket);
          JavaOnlyMap data = event(id);

          data.putInt("code", code);
          data.putString("reason", reason);
          emit("websocketClosed", data);
        }

        @Override public void onFailure(WebSocket socket, Throwable error, Response response) {
          sockets.remove(id, socket);
          failed(id, error.getMessage());
        }
      });

      sockets.put(id, socket);
    } catch (Exception error) {
      failed(id, error.getMessage());
    }
  }

  @LynxMethod
  public void send(String message, double socketID) {
    WebSocket socket = sockets.get((int) socketID);

    if (socket != null) socket.send(message);
  }

  @LynxMethod
  public void close(double code, String reason, double socketID) {
    WebSocket socket = sockets.remove((int) socketID);

    if (socket != null) socket.close((int) code, reason);
  }

  @Override public void destroy() {
    destroyed = true;
    store.removeListener(changed);
    for (WebSocket socket : sockets.values()) socket.cancel();
    sockets.clear();
    client.dispatcher().executorService().shutdown();
    super.destroy();
  }
}
