package bg.sofia.uni.fmi.mjt.splitwise.command.executors.account;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;
import bg.sofia.uni.fmi.mjt.splitwise.utils.CommandConstants;
import bg.sofia.uni.fmi.mjt.splitwise.utils.IndexesInCommands;
import bg.sofia.uni.fmi.mjt.splitwise.utils.Messages;
import bg.sofia.uni.fmi.mjt.splitwise.validator.Validator;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.channels.SelectionKey;

public class AccountExecutorAPI implements AccountExecutor {

    @Override
    public String getHistory(Account account) {
        String fileName = account.username() + ".txt";
        try (var writer = new BufferedWriter((new FileWriter(fileName, false)))) {

            account.logs().forEach(log -> {
                try {
                    writer.write(log);
                    writer.write(System.lineSeparator());
                    writer.flush();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
        } catch (IOException e) {
            return Messages.LOGS_NOT_SAVED;
        }

        return String.format(Messages.LOGS_SUCCESSFULLY_SAVED, fileName);
    }

    @Override
    public String helpLoggedIn() {
        return Messages.HELP_LOGGED_IN;
    }

    @Override
    public String helpLoggedOut() {
        return Messages.HELP_LOGGED_OUT;
    }

    @Override
    public String logout(SelectionKey key) {
        key.attach(null);
        return Messages.LOGGED_OUT_SUCCESSFULLY;
    }

    @Override
    public String register(String[] arguments, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (arguments.length != IndexesInCommands.ARGUMENTS_COUNT_FOR_REGISTER_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.REGISTER,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_REGISTER_COMMAND,
                CommandConstants.REGISTER + " <first name> <last name> <username> <password>");
        }

        String firstName = arguments[IndexesInCommands.FIRSTNAME_INDEX_IN_REGISTER];
        String lastName = arguments[IndexesInCommands.LASTNAME_INDEX_IN_REGISTER];
        String username = arguments[IndexesInCommands.USERNAME_INDEX_IN_REGISTER];
        String password = arguments[IndexesInCommands.PASSWORD_INDEX_IN_REGISTER];
        if (storage.getAccount(username) != null) {
            return String.format(Messages.USER_ALREADY_EXISTS, username);
        }

        if (!Validator.validatePassword(password)) {
            return Messages.INVALID_PASSWORD;
        }

        Account account = new Account(firstName, lastName, username, password);
        storage.addAccount(account);
        dataBaseAPI.saveAccount(account);
        return String.format(Messages.REGISTERED_SUCCESSFULLY, username);
    }

    @Override
    public String login(String[] arguments, SelectionKey key, Storage storage) {
        if (arguments.length != IndexesInCommands.ARGUMENTS_COUNT_FOR_LOGIN_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.LOGIN,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_LOGIN_COMMAND,
                CommandConstants.LOGIN + " <username> <password>");
        }

        String username = arguments[IndexesInCommands.USERNAME_INDEX_IN_LOGIN];
        String password = arguments[IndexesInCommands.PASSWORD_INDEX_IN_LOGIN];
        Account account = storage.getAccount(username, password);
        if (account == null) {
            return Messages.WRONG_USERNAME_OR_PASSWORD;
        }
        key.attach(account);
        return Messages.LOGIN_SUCCESSFULLY;
    }

    @Override
    public String status(Account account) {
        return account.status();
    }
}
