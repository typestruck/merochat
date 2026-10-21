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
