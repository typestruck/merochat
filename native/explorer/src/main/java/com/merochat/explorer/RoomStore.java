package com.merochat.explorer;

import android.content.Context;
import android.database.Cursor;
import androidx.annotation.NonNull;
import androidx.room.Dao;
import androidx.room.Database;
import androidx.room.Entity;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.PrimaryKey;
import androidx.room.Query;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.Transaction;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/** Stores the Argonaut representation of ImModel.user and ImModel.contacts. */
@Database(entities = {RoomStore.UserEntity.class, RoomStore.ContactEntity.class, RoomStore.SuggestionEntity.class, RoomStore.PostEntity.class, RoomStore.AskEntity.class, RoomStore.PraiseEntity.class, RoomStore.ThemeEntity.class},
    version = 8, exportSchema = true)
public abstract class RoomStore extends RoomDatabase {
  private static RoomStore instance;
  static final ExecutorService executor = Executors.newSingleThreadExecutor();

  static synchronized RoomStore get(Context context) {
    if (instance == null) {
      instance = Room.databaseBuilder(context.getApplicationContext(), RoomStore.class,
          "merochat-im.db").addMigrations(new Migration(1, 2) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("CREATE TABLE IF NOT EXISTS bootstrap (id INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
            }
          }, new Migration(2, 3) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("CREATE TABLE IF NOT EXISTS suggestions (id INTEGER NOT NULL, position INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
              database.execSQL("CREATE TABLE IF NOT EXISTS changelogs (id INTEGER NOT NULL, position INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
              try (Cursor cursor = database.query("SELECT json FROM bootstrap WHERE id = 0")) {
                if (cursor.moveToFirst()) {
                  JSONObject bootstrap = new JSONObject(cursor.getString(0));

                  for (String table : new String[] {"suggestions", "changelogs"}) {
                    JSONArray rows = bootstrap.getJSONArray(table);

                    for (int i = 0; i < rows.length(); i++) {
                      JSONObject row = rows.getJSONObject(i);

                      database.execSQL("INSERT OR REPLACE INTO " + table + " (id, position, json) VALUES (?, ?, ?)",
                          new Object[] {row.getInt("id"), i, row.toString()});
                    }
                  }
                }
              } catch (JSONException error) {
                throw new IllegalStateException("Invalid Room bootstrap data", error);
              }
              database.execSQL("DROP TABLE bootstrap");
            }
          }, new Migration(3, 4) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("DROP TABLE changelogs");
            }
          }, new Migration(4, 5) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("CREATE TABLE IF NOT EXISTS profile (id INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
            }
          }, new Migration(5, 6) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("DELETE FROM users");
              database.execSQL("DELETE FROM contacts");
              database.execSQL("DELETE FROM suggestions");
              database.execSQL("DELETE FROM profile");
            }
          }, new Migration(6, 7) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              for (String table : new String[] {"posts", "asks", "praise"}) {
                database.execSQL("CREATE TABLE IF NOT EXISTS " + table + " (id INTEGER NOT NULL, position INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
              }
              try (Cursor cursor = database.query("SELECT json FROM profile WHERE id = 0")) {
                if (cursor.moveToFirst()) {
                  JSONObject profile = new JSONObject(cursor.getString(0));

                  for (String table : new String[] {"posts", "asks", "praise"}) {
                    JSONArray rows = profile.getJSONArray(table);

                    for (int i = 0; i < rows.length(); i++) {
                      JSONObject row = rows.getJSONObject(i);

                      database.execSQL("INSERT OR REPLACE INTO " + table + " (id, position, json) VALUES (?, ?, ?)",
                          new Object[] {row.getInt("id"), i, row.toString()});
                    }
                  }
                }
              } catch (JSONException error) {
                throw new IllegalStateException("Invalid Room profile data", error);
              }
              database.execSQL("DROP TABLE profile");
            }
          }, new Migration(7, 8) {
            @Override public void migrate(@NonNull SupportSQLiteDatabase database) {
              database.execSQL("CREATE TABLE IF NOT EXISTS theme (id INTEGER NOT NULL, json TEXT NOT NULL, PRIMARY KEY(id))");
            }
          }).build();
    }
    return instance;
  }

  public abstract ImDao im();

  @Entity(tableName = "users")
  public static class UserEntity {
    @PrimaryKey public int id;
    @NonNull public String json = "";
  }

  @Entity(tableName = "contacts")
  public static class ContactEntity {
    @PrimaryKey public int userId;
    @NonNull public String json = "";
  }

  @Entity(tableName = "suggestions")
  public static class SuggestionEntity {
    @PrimaryKey public int id;
    public int position;
    @NonNull public String json = "";
  }

  @Entity(tableName = "posts")
  public static class PostEntity {
    @PrimaryKey public int id;
    public int position;
    @NonNull public String json = "";
  }

  @Entity(tableName = "asks")
  public static class AskEntity {
    @PrimaryKey public int id;
    public int position;
    @NonNull public String json = "";
  }

  @Entity(tableName = "praise")
  public static class PraiseEntity {
    @PrimaryKey public int id;
    public int position;
    @NonNull public String json = "";
  }

  @Entity(tableName = "theme")
  public static class ThemeEntity {
    @PrimaryKey public int id;
    @NonNull public String json = "";
  }

  @Dao
  public interface ImDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertTheme(ThemeEntity theme);
    @Query("SELECT * FROM theme WHERE id = 0") ThemeEntity selectTheme();

    @Transaction
    default void insertPosts(List<PostEntity> rows) {
      clearPosts();
      insertPostsRows(rows);
    }

    @Query("DELETE FROM posts") void clearPosts();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertPostsRows(List<PostEntity> rows);
    @Query("SELECT * FROM posts ORDER BY position") List<PostEntity> selectPosts();

    @Transaction
    default void insertAsks(List<AskEntity> rows) {
      clearAsks();
      insertAsksRows(rows);
    }

    @Query("DELETE FROM asks") void clearAsks();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertAsksRows(List<AskEntity> rows);
    @Query("SELECT * FROM asks ORDER BY position") List<AskEntity> selectAsks();

    @Transaction
    default void insertPraise(List<PraiseEntity> rows) {
      clearPraise();
      insertPraiseRows(rows);
    }

    @Query("DELETE FROM praise") void clearPraise();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertPraiseRows(List<PraiseEntity> rows);
    @Query("SELECT * FROM praise ORDER BY position") List<PraiseEntity> selectPraise();

    @Transaction
    default void insertUser(UserEntity user) {
      clearUsers();
      insertUserRow(user);
    }

    @Query("DELETE FROM users") void clearUsers();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertUserRow(UserEntity user);
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertContacts(List<ContactEntity> contacts);
    @Transaction
    default void insertSuggestions(List<SuggestionEntity> rows) {
      clearSuggestions();
      insertSuggestionsRows(rows);
    }

    @Query("DELETE FROM suggestions") void clearSuggestions();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertSuggestionsRows(List<SuggestionEntity> rows);
    @Query("SELECT * FROM suggestions ORDER BY position") List<SuggestionEntity> selectSuggestions();

    @Query("SELECT * FROM users LIMIT 1") UserEntity selectUser();
    @Query("SELECT * FROM contacts ORDER BY userId") List<ContactEntity> selectContacts();
  }
}
