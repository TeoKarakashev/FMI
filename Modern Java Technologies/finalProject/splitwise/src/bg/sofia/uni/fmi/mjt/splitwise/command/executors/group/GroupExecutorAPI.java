package bg.sofia.uni.fmi.mjt.splitwise.command.executors.group;

import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.GroupConnection;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;
import bg.sofia.uni.fmi.mjt.splitwise.utils.CommandConstants;
import bg.sofia.uni.fmi.mjt.splitwise.utils.IndexesInCommands;
import bg.sofia.uni.fmi.mjt.splitwise.utils.Messages;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class GroupExecutorAPI implements GroupExecutor {

    private static final int NEW_CONNECTION_MONEY = 0;
    @Override
    public String payedGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length != IndexesInCommands.ARGUMENTS_COUNT_FOR_PAYED_GROUP_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.PAYED_GROUP,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_PAYED_GROUP_COMMAND,
                CommandConstants.PAYED_GROUP + " <group name> <username> <amount>");
        }

        String groupName = args[IndexesInCommands.GROUP_NAME_INDEX_IN_PAYED_GROUP];

        double amount;
        try {
            amount = Double.parseDouble(args[IndexesInCommands.AMOUNT_INDEX_IN_PAYED_GROUP]);
        } catch (NumberFormatException e) {
            return Messages.INVALID_AMOUNT_PROVIDED;
        }


        GroupConnection groupConnection = account.getGroupConnection(groupName);

        if (groupConnection == null) {
            return String.format(Messages.GROUP_DOES_NOT_EXIST, groupName);
        }

        if (!groupConnection.owner().equals(account.username())) {
            return Messages.NOT_PERMITTED_OPERATION;
        }

        String username = args[IndexesInCommands.USERNAME_INDEX_IN_PAYED_GROUP];
        if (!groupConnection.containsUser(username)) {
            return String.format(Messages.USER_DOES_NOT_EXIST_IN_GROUP, username, groupName);
        }


        Account accountToPay = storage.getAccount(username);
        account.payGroup(accountToPay, groupConnection, amount, groupName, dataBaseAPI);
        return String.format(Messages.PAYED_SUCCESSFULLY_GROUP, accountToPay.firstName(), accountToPay.lastName(),
            accountToPay.username(), amount, groupName) + System.lineSeparator() +
            String.format(Messages.CURRENT_STATUS, account.getGroupStatus(groupConnection, true));
    }

    @Override
    public String splitGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length < IndexesInCommands.ARGUMENTS_COUNT_FOR_SPLIT_GROUP_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.SPLIT_GROUP,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_SPLIT_GROUP_COMMAND,
                CommandConstants.SPLIT_GROUP + " <group-name> <amount> <reason>");
        }

        String groupName = args[IndexesInCommands.GROUP_NAME_INDEX_IN_SPLIT_GROUP];


        double amount;
        try {
            amount = Double.parseDouble(args[IndexesInCommands.AMOUNT_INDEX_IN_SPLIT_GROUP]);
        } catch (NumberFormatException e) {
            return Messages.INVALID_AMOUNT_PROVIDED;
        }


        GroupConnection groupConnection = account.getGroupConnection(groupName);

        if (groupConnection == null) {
            return String.format(Messages.GROUP_DOES_NOT_EXIST, groupName);
        }

        if (!groupConnection.owner().equals(account.username())) {
            return Messages.NOT_PERMITTED_OPERATION;
        }

        List<String> reasonList =
            new ArrayList<>(
                Arrays.asList(args).subList(IndexesInCommands.REASON_INDEX_IN_SPLIT_GROUP, args.length));

        account.splitGroup(groupName, groupConnection, amount, reasonList, storage, dataBaseAPI);

        return
            String.format(Messages.SPLIT_SUCCESSFULLY_GROUP, amount, groupName) + System.lineSeparator() +
                String.format(Messages.CURRENT_STATUS, account.getGroupStatus(groupConnection, true));
    }

    @Override
    public String createGroup(String[] args, Account account, Storage storage, LocalDataBaseAPI dataBaseAPI) {
        if (args.length < IndexesInCommands.ARGUMENTS_COUNT_FOR_CREATE_GROUP_COMMAND) {
            return String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.CREATE_GROUP,
                IndexesInCommands.ARGUMENTS_COUNT_FOR_CREATE_GROUP_COMMAND,
                CommandConstants.CREATE_GROUP + " <group_name> <username1> <username2> ... <usernameN>");
        }

        String groupName = args[IndexesInCommands.GROUP_NAME_INDEX_IN_CREATE_GROUP];

        if (account.getGroupConnection(groupName) != null) {
            return String.format(Messages.GROUP_ALREADY_EXISTS, groupName);
        }

        Set<Account> friends = new HashSet<>();
        for (int i = IndexesInCommands.GROUP_FIRST_MEMBER_INDEX_IN_CREATE_GROUP; i < args.length; i++) {
            String username = args[i];
            Account friend = storage.getAccount(username);
            if (friend == null) {
                return String.format(Messages.USER_DOES_NOT_EXIST, username);
            }

            if (friend.username().equals(account.username())) {
                return Messages.CANNOT_ADD_YOURSELF;
            }
            friends.add(friend);
        }

        if (friends.size() == 0) {
            return Messages.NO_FRIENDS_IN_COMMAND_ADD_GROUP;
        }

        account.createGroup(friends, groupName);
        Map<String, Double> friendsMap = new HashMap<>();
        for (Account friend : friends) {
            friendsMap.put(friend.username(), 0.0);
        }

        dataBaseAPI.saveGroup(groupName, account.username(), NEW_CONNECTION_MONEY, friendsMap);
        return String.format(Messages.GROUP_CREATED_SUCCESSFULLY, groupName);
    }
}
