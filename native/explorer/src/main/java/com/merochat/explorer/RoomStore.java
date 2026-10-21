package com.merochat.explorer;

import android.content.Context;
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
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/** Stores the Argonaut representation of ImModel.user and ImModel.contacts. */
@Database(entities = {RoomStore.UserEntity.class, RoomStore.ContactEntity.class},
    version = 1, exportSchema = true)
public abstract class RoomStore extends RoomDatabase {
  private static RoomStore instance;
  static final ExecutorService executor = Executors.newSingleThreadExecutor();

  static synchronized RoomStore get(Context context) {
    if (instance == null) {
      instance = Room.databaseBuilder(context.getApplicationContext(), RoomStore.class,
          "merochat-im.db").build();
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

  @Dao
  public interface ImDao {
    @Transaction
    default void insertUser(UserEntity user) {
      clearUsers();
      insertUserRow(user);
    }

    @Query("DELETE FROM users") void clearUsers();
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertUserRow(UserEntity user);
    @Insert(onConflict = OnConflictStrategy.REPLACE) void insertContacts(List<ContactEntity> contacts);
    @Query("SELECT * FROM users LIMIT 1") UserEntity selectUser();
    @Query("SELECT * FROM contacts ORDER BY userId") List<ContactEntity> selectContacts();
  }
}
