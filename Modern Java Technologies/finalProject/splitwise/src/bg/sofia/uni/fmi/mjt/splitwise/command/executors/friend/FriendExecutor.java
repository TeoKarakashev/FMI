package bg.sofia.uni.fmi.mjt.splitwise.command.executors.friend;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;

public interface FriendExecutor {
    String payed(String[] arguments, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
    String addFriend(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
    String split(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
}
