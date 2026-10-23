package com.merochat.explorer;

import com.lynx.jsbridge.LynxContextModule;
import com.lynx.jsbridge.LynxMethod;
import com.lynx.react.bridge.Callback;
import com.lynx.tasm.behavior.LynxContext;
import java.util.ArrayList;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONObject;

/** Async bridge: callbacks receive (error, value), with null on success. */
public final class RoomModule extends LynxContextModule {
  private final RoomStore store;

  public RoomModule(LynxContext context) {
    super(context);
    store = RoomStore.get(context);
  }

  private interface Operation {
    Object run() throws Exception;
  }

  private void execute(Callback callback, Operation operation) {
    RoomStore.executor.execute(() -> {
      Object result;
      try {
        result = operation.run();
      } catch (Exception error) {
        callback.invoke(error.toString(), null);
        return;
      }
      callback.invoke(null, result);
    });
  }

  @LynxMethod
  public void insert(String entity, String json, Callback callback) {
    execute(callback, () -> {
      switch (entity) {
        case "theme":
          RoomStore.ThemeEntity theme = new RoomStore.ThemeEntity();

          theme.id = 0;
          theme.json = json;
          store.im().insertTheme(theme);
          break;
        case "posts":
          JSONArray postsValues = new JSONArray(json);
          List<RoomStore.PostEntity> posts = new ArrayList<>();

          for (int i = 0; i < postsValues.length(); i++) {
            JSONObject row = postsValues.getJSONObject(i);
            RoomStore.PostEntity item = new RoomStore.PostEntity();

            item.id = row.getInt("id");
            item.position = i;
            item.json = row.toString();
            posts.add(item);
          }
          store.im().insertPosts(posts);
          break;
        case "asks":
          JSONArray asksValues = new JSONArray(json);
          List<RoomStore.AskEntity> asks = new ArrayList<>();

          for (int i = 0; i < asksValues.length(); i++) {
            JSONObject row = asksValues.getJSONObject(i);
            RoomStore.AskEntity item = new RoomStore.AskEntity();

            item.id = row.getInt("id");
            item.position = i;
            item.json = row.toString();
            asks.add(item);
          }
          store.im().insertAsks(asks);
          break;
        case "praise":
          JSONArray praiseValues = new JSONArray(json);
          List<RoomStore.PraiseEntity> praise = new ArrayList<>();

          for (int i = 0; i < praiseValues.length(); i++) {
            JSONObject row = praiseValues.getJSONObject(i);
            RoomStore.PraiseEntity item = new RoomStore.PraiseEntity();

            item.id = row.getInt("id");
            item.position = i;
            item.json = row.toString();
            praise.add(item);
          }
          store.im().insertPraise(praise);
          break;
        case "suggestions":
          JSONArray suggestionsValues = new JSONArray(json);
          List<RoomStore.SuggestionEntity> suggestions = new ArrayList<>();

          for (int i = 0; i < suggestionsValues.length(); i++) {
            JSONObject row = suggestionsValues.getJSONObject(i);
            RoomStore.SuggestionEntity suggestion = new RoomStore.SuggestionEntity();

            suggestion.id = row.getInt("id");
            suggestion.position = i;
            suggestion.json = row.toString();
            suggestions.add(suggestion);
          }
          store.im().insertSuggestions(suggestions);
          break;
        case "user":
          JSONObject value = new JSONObject(json);
          RoomStore.UserEntity user = new RoomStore.UserEntity();
          user.id = value.getInt("id");
          user.json = json;
          store.im().insertUser(user);
          break;
        case "contacts":
          JSONArray values = new JSONArray(json);
          List<RoomStore.ContactEntity> contacts = new ArrayList<>();
          for (int i = 0; i < values.length(); i++) {
            JSONObject contactValue = values.getJSONObject(i);
            RoomStore.ContactEntity contact = new RoomStore.ContactEntity();
            contact.userId = contactValue.getJSONObject("user").getInt("id");
            contact.json = contactValue.toString();
            contacts.add(contact);
          }
          store.im().insertContacts(contacts);
          break;
        default:
          throw new IllegalArgumentException("Unknown Room entity: " + entity);
      }
      return null;
    });
  }

  @LynxMethod
  public void select(String entity, Callback callback) {
    execute(callback, () -> {
      switch (entity) {
        case "theme":
          RoomStore.ThemeEntity theme = store.im().selectTheme();

          return theme == null ? "null" : theme.json;
        case "posts":
          JSONArray posts = new JSONArray();

          for (RoomStore.PostEntity row : store.im().selectPosts()) {
            posts.put(new JSONObject(row.json));
          }

          return posts.toString();
        case "asks":
          JSONArray asks = new JSONArray();

          for (RoomStore.AskEntity row : store.im().selectAsks()) {
            asks.put(new JSONObject(row.json));
          }

          return asks.toString();
        case "praise":
          JSONArray praise = new JSONArray();

          for (RoomStore.PraiseEntity row : store.im().selectPraise()) {
            praise.put(new JSONObject(row.json));
          }

          return praise.toString();
        case "suggestions":
          JSONArray suggestions = new JSONArray();

          for (RoomStore.SuggestionEntity row : store.im().selectSuggestions()) {
            suggestions.put(new JSONObject(row.json));
          }

          return suggestions.toString();
        case "user":
          RoomStore.UserEntity user = store.im().selectUser();
          return user == null ? "null" : user.json;
        case "contacts":
          JSONArray contacts = new JSONArray();
          for (RoomStore.ContactEntity contact : store.im().selectContacts()) {
            contacts.put(new JSONObject(contact.json));
          }
          return contacts.length() == 0 ? "null" : contacts.toString();
        default:
          throw new IllegalArgumentException("Unknown Room entity: " + entity);
      }
    });
  }
}
