package bg.sofia.uni.fmi.mjt.splitwise.command.executors.group;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;

public interface GroupExecutor {

    String payedGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
    String splitGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
    String createGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI);
}
