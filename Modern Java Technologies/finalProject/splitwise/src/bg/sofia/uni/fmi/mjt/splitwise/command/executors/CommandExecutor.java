package bg.sofia.uni.fmi.mjt.splitwise.command.executors;

import bg.sofia.uni.fmi.mjt.splitwise.command.Command;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;

import java.nio.channels.SelectionKey;

public interface CommandExecutor {

    String execute(Command cmd, SelectionKey key);


}
