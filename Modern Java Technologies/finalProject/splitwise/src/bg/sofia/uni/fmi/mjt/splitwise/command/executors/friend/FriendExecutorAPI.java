package bg.sofia.uni.fmi.mjt.splitwise.command.executors.friend;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;
import bg.sofia.uni.fmi.mjt.splitwise.utils.CommandConstants;
import bg.sofia.uni.fmi.mjt.splitwise.utils.IndexesInCommands;
import bg.sofia.uni.fmi.mjt.splitwise.utils.Messages;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FriendExecutorAPI implements FriendExecutor {

    private static final int NEW_CONNECTION_MONEY = 0;
    @Override
    public String payed(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length != IndexesInCommands.ARGUMENTS_COUNT_FOR_PAYED_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.PAYED,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_PAYED_COMMAND,
                CommandConstants.PAYED + " <username> <amount>");
        }

        String username = args[IndexesInCommands.USERNAME_INDEX_IN_PAYED];

        double amount;
        try {
            amount = Double.parseDouble(args[IndexesInCommands.AMOUNT_INDEX_IN_PAYED]);
        } catch (NumberFormatException e) {
            return Messages.INVALID_AMOUNT_PROVIDED;
        }

        Account friend = storage.getAccount(username);

        if (friend == null) {
            return String.format(Messages.USER_DOES_NOT_EXIST, username);
        }

        if (account.memberOfFriendConnection().containsKey(friend.username())) {
            return Messages.NOT_PERMITTED_OPERATION;
        }

        if (!account.ownerToFriendConnection().containsKey(friend.username())) {
            return String.format(Messages.NOT_FRIENDS, username);
        }

        account.payed(friend, amount, dataBaseAPI);

        return
            String.format(Messages.PAYED_SUCCESSFULLY, friend.firstName(), friend.lastName(), friend.username(), amount,
                "BGN") + System.lineSeparator() +
                String.format(Messages.CURRENT_STATUS, account.getStatus(friend.username(), true));
    }

    @Override
    public String addFriend(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length != IndexesInCommands.ARGUMENTS_COUNT_FOR_ADD_FRIEND_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.ADD_FRIEND,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_ADD_FRIEND_COMMAND,
                CommandConstants.ADD_FRIEND + " <username>");
        }

        String username = args[IndexesInCommands.USERNAME_INDEX_IN_ADD_FRIEND];
        Account friend = storage.getAccount(username);
        if (friend == null) {
            return String.format(Messages.USER_DOES_NOT_EXIST, username);
        }

        if (friend.username().equals(account.username())) {
            return Messages.CANNOT_ADD_YOURSELF;
        }

        if (account.ownerToFriendConnection().containsKey(friend.username()) ||
            account.memberOfFriendConnection().containsKey(friend.username())) {
            return String.format(Messages.ALREADY_FRIENDS, username);
        }

        account.addFriend(friend);
        dataBaseAPI.saveFriend(account.username(), friend.username(), NEW_CONNECTION_MONEY, NEW_CONNECTION_MONEY);
        return String.format(Messages.ADDED_FRIEND_SUCCESSFULLY, username);
    }

    @Override
    public String split(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length < IndexesInCommands.ARGUMENTS_COUNT_FOR_SPLIT_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.SPLIT,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_SPLIT_COMMAND,
                CommandConstants.SPLIT + " <username> <amount> <reason_for_payment>");
        }

        double amount;
        try {
            amount = Double.parseDouble(args[IndexesInCommands.AMOUNT_INDEX_IN_SPLIT]);
        } catch (NumberFormatException e) {
            return Messages.INVALID_AMOUNT_PROVIDED;
        }
        String username = args[IndexesInCommands.USERNAME_INDEX_IN_SPLIT];
        Account friend = storage.getAccount(username);

        if (friend == null) {
            return String.format(Messages.USER_DOES_NOT_EXIST, username);
        }

        if (account.memberOfFriendConnection().containsKey(friend.username())) {
            return Messages.NOT_PERMITTED_OPERATION;
        }

        if (!account.ownerToFriendConnection().containsKey(friend.username())) {
            return String.format(Messages.NOT_FRIENDS, username);
        }

        List<String> reasonList =
            new ArrayList<>(Arrays.asList(args).subList(IndexesInCommands.REASON_INDEX_IN_SPLIT, args.length));

        account.split(friend, amount, reasonList, dataBaseAPI);

        return String.format(Messages.SPLIT_SUCCESSFULLY, amount, "BGN", friend.firstName(), friend.lastName()) +
            System.lineSeparator() +
            String.format(Messages.CURRENT_STATUS, account.getStatus(friend.username(), true));
    }



}
