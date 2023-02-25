package bg.sofia.uni.fmi.mjt.splitwise.command.executors;

import bg.sofia.uni.fmi.mjt.splitwise.command.Command;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.account.AccountExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.account.AccountExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.friend.FriendExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.friend.FriendExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.group.GroupExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.group.GroupExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;
import bg.sofia.uni.fmi.mjt.splitwise.utils.CommandConstants;
import bg.sofia.uni.fmi.mjt.splitwise.utils.Messages;

import java.nio.channels.SelectionKey;

public class CommandExecutorAPI implements CommandExecutor {
    private final Storage storage;
    private final LocalDataBaseAPI dataBaseAPI;
    private boolean isLoaded;
    private final AccountExecutor accountExecutor;
    private final GroupExecutor groupExecutor;
    private final FriendExecutor friendExecutor;

    public CommandExecutorAPI(Storage storage) {
        this.storage = storage;
        this.isLoaded = false;
        this.dataBaseAPI = new LocalDataBaseAPI();
        this.accountExecutor = new AccountExecutorAPI();
        this.groupExecutor = new GroupExecutorAPI();
        this.friendExecutor = new FriendExecutorAPI();
    }

    @Override
    public String execute(Command cmd, SelectionKey key) {
        if (!isLoaded) {
            dataBaseAPI.load(storage);
            isLoaded = true;
        }
        Account account = (Account) key.attachment();

        if (account == null) {
            return switch (cmd.command()) {
                case CommandConstants.LOGIN -> accountExecutor.login(cmd.arguments(), key, storage);
                case CommandConstants.REGISTER -> accountExecutor.register(cmd.arguments(), storage, dataBaseAPI);
                case CommandConstants.HELP -> accountExecutor.helpLoggedOut();
                default -> Messages.NOT_LOGGED_IN;
            };
        }

        return switch (cmd.command()) {
            case CommandConstants.ADD_FRIEND ->
                friendExecutor.addFriend(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.CREATE_GROUP ->
                groupExecutor.createGroup(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.SPLIT -> friendExecutor.split(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.PAYED -> friendExecutor.payed(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.SPLIT_GROUP ->
                groupExecutor.splitGroup(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.PAYED_GROUP ->
                groupExecutor.payedGroup(cmd.arguments(), account, storage, dataBaseAPI);
            case CommandConstants.STATUS -> accountExecutor.status(account);
            case CommandConstants.LOGOUT -> accountExecutor.logout(key);
            case CommandConstants.GET_LOGS -> accountExecutor.getHistory(account);
            case CommandConstants.HELP -> accountExecutor.helpLoggedIn();
            default -> Messages.INVALID_COMMAND_MESSAGE.formatted(cmd.command());
        };
    }
}
