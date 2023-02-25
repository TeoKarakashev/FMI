package bg.sofia.uni.fmi.mjt.splitwise.command.executors.account;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;

import java.nio.channels.SelectionKey;

public interface AccountExecutor {
    String getHistory(Account account);
    String helpLoggedIn();
    String helpLoggedOut();
    String logout(SelectionKey key);
    String register(String[] arguments, Storage storage, LocalDataBaseAPI dataBaseAPI);
    String login(String[] arguments, SelectionKey key, Storage storage);
    String status(Account account);
}

