package com.merochat.explorer;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Handler;
import android.os.Looper;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.HttpUrl;

/** The native HTTP client and session bridge share this cookie store. */
final class SessionStore {
  private static SessionStore instance;
  static synchronized SessionStore get(Context context) {
    if (instance == null) instance = new SessionStore(context.getApplicationContext());
    return instance;
  }

  private final SharedPreferences preferences;
  private final Handler handler = new Handler(Looper.getMainLooper());
  private final List<Cookie> cookies = new ArrayList<>();
  private final Set<Runnable> listeners = new HashSet<>();
  private final Runnable expire = this::expireCookies;
  private HttpUrl origin;
  private long generation;

  private SessionStore(Context context) {
    preferences = context.getSharedPreferences("merochat-native-cookies", Context.MODE_PRIVATE);
  }

  synchronized void configure(String value) {
    HttpUrl url = HttpUrl.get(value);
    if (!url.encodedPath().equals("/") || url.query() != null || url.fragment() != null ||
        !url.username().isEmpty() || !url.password().isEmpty()) {
      throw new IllegalArgumentException("Session URL must be an HTTP(S) origin");
    }
    if (url.equals(origin)) return;
    generation++;
    origin = url;
    cookies.clear();
    for (String encoded : preferences.getStringSet(origin.toString(), Collections.emptySet())) {
      Cookie cookie = Cookie.parse(origin, encoded);
      if (cookie != null && cookie.expiresAt() > System.currentTimeMillis()) cookies.add(cookie);
    }
    scheduleExpiry();
  }

  synchronized boolean owns(HttpUrl url) {
    return origin != null && origin.scheme().equals(url.scheme()) &&
        origin.host().equals(url.host()) && origin.port() == url.port();
  }

  synchronized boolean authenticated() {
    expireCookies();
    for (Cookie cookie : cookies) {
      if (cookie.name().equals("merochat") && !cookie.value().isEmpty() && cookie.matches(origin)) return true;
    }
    return false;
  }

  synchronized long generation() { return generation; }
  synchronized void addListener(Runnable listener) { listeners.add(listener); }
  synchronized void removeListener(Runnable listener) { listeners.remove(listener); }

  synchronized void clear(long expectedGeneration) {
    if (generation != expectedGeneration) return;
    generation++;
    cookies.clear();
    persist();
    changed();
  }

  CookieJar jar(long expectedGeneration) {
    return new CookieJar() {
      @Override public List<Cookie> loadForRequest(HttpUrl url) {
        synchronized (SessionStore.this) {
          expireCookies();
          if (generation != expectedGeneration || !owns(url)) return Collections.emptyList();
          List<Cookie> result = new ArrayList<>();
          for (Cookie cookie : cookies) if (cookie.matches(url)) result.add(cookie);
          return result;
        }
      }
      @Override public void saveFromResponse(HttpUrl url, List<Cookie> received) {
        synchronized (SessionStore.this) {
          if (generation != expectedGeneration || !owns(url)) return;
          boolean deletedSession = false;
          for (Cookie cookie : received) {
            cookies.removeIf(old -> old.name().equals(cookie.name()) &&
                old.domain().equals(cookie.domain()) && old.path().equals(cookie.path()));
            if (cookie.expiresAt() > System.currentTimeMillis() && !cookie.value().isEmpty()) cookies.add(cookie);
            else if (cookie.name().equals("merochat")) deletedSession = true;
          }
          if (deletedSession) generation++;
          persist();
          changed();
        }
      }
    };
  }

  private synchronized void expireCookies() {
    boolean lostSession = cookies.stream().anyMatch(cookie -> cookie.name().equals("merochat") &&
        cookie.expiresAt() <= System.currentTimeMillis());
    if (cookies.removeIf(cookie -> cookie.expiresAt() <= System.currentTimeMillis())) {
      if (lostSession) generation++;
      persist();
      changed();
    }
  }

  private void persist() {
    if (origin == null) return;
    Set<String> values = new HashSet<>();
    // Session cookies intentionally do not survive an app process restart.
    for (Cookie cookie : cookies) if (cookie.persistent()) values.add(cookie.toString());
    preferences.edit().putStringSet(origin.toString(), values).apply();
  }

  private void changed() {
    scheduleExpiry();
    for (Runnable listener : listeners) handler.post(listener);
  }

  private void scheduleExpiry() {
    handler.removeCallbacks(expire);
    long next = Long.MAX_VALUE;
    for (Cookie cookie : cookies) next = Math.min(next, cookie.expiresAt());
    if (next != Long.MAX_VALUE) handler.postDelayed(expire, Math.max(1, next - System.currentTimeMillis()));
  }
}
