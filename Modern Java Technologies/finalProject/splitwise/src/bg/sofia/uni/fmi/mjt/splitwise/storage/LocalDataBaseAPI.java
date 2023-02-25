package bg.sofia.uni.fmi.mjt.splitwise.storage;

import bg.sofia.uni.fmi.mjt.splitwise.exceptions.MissingFileException;
import bg.sofia.uni.fmi.mjt.splitwise.exceptions.UpdateDataBaseException;
import bg.sofia.uni.fmi.mjt.splitwise.utils.IndexesInCommands;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static java.nio.file.StandardOpenOption.APPEND;

public class LocalDataBaseAPI {

    private static final String ACCOUNTS_PATH = "accounts.txt";
    private static final String FRIENDS_PATH = "friends.txt";
    private static final String GROUPS_PATH = "groups.txt";
    private static final String LOGS_PATH = "logs.txt";

    public void load(Storage storage) {
        loadAccounts(storage);
        loadLogs(storage);
        loadFriends(storage);
        loadGroups(storage);
    }

    private void loadGroups(Storage storage) {

        Path of = Path.of(GROUPS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String groupName = tokens[IndexesInCommands.LOAD_GROUPS_GROUP_NAME_INDEX];
                String ownerName = tokens[IndexesInCommands.LOAD_GROUPS_OWNER_USERNAME_INDEX];
                double payedByOwner = Double.parseDouble(tokens[IndexesInCommands.LOAD_GROUPS_PAYED_BY_OWNER_INDEX]);
                Set<Account> groupMembers = new HashSet<>();
                Map<String, Double> payedByGroup = new HashMap<>();
                for (int i = IndexesInCommands.LOAD_GROUPS_FIRST_MEMBER_INDEX; i < tokens.length; i++) {
                    groupMembers.add(storage.getAccount(tokens[i]));
                    payedByGroup.put(tokens[i], Double.parseDouble(tokens[++i]));
                }
                storage.getAccount(ownerName).createGroup(groupMembers, groupName, payedByOwner, payedByGroup);
            }
        } catch (FileNotFoundException e) {
            throw new MissingFileException("File " + GROUPS_PATH + " is missing");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void loadFriends(Storage storage) {
        Path of = Path.of(FRIENDS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String username = tokens[IndexesInCommands.LOAD_FRIENDS_USERNAME_INDEX];
                String friendUsername = tokens[IndexesInCommands.LOAD_FRIENDS_FRIEND_USERNAME_INDEX];
                double payedByOwner = Double.parseDouble(tokens[IndexesInCommands.LOAD_FRIENDS_PAYED_BY_OWNER_INDEX]);
                double payedByFriend = Double.parseDouble(tokens[IndexesInCommands.LOAD_FRIENDS_PAYED_BY_FRIEND_INDEX]);

                storage.getAccount(username)
                    .addFriend(storage.getAccount(friendUsername), payedByOwner, payedByFriend);
            }
        } catch (FileNotFoundException e) {
            throw new MissingFileException("File " + FRIENDS_PATH + " is missing");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void loadAccounts(Storage storage) {
        Path of = Path.of(ACCOUNTS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String username = tokens[IndexesInCommands.LOAD_DATABASE_USERNAME_INDEX];
                String password = tokens[IndexesInCommands.LOAD_DATABASE_PASSWORD_INDEX];
                String firstName = tokens[IndexesInCommands.LOAD_DATABASE_FIRSTNAME_INDEX];
                String lastName = tokens[IndexesInCommands.LOAD_DATABASE_LASTNAME_INDEX];
                Account account = new Account(firstName, lastName, username, password);
                storage.addAccount(account);
            }

        } catch (FileNotFoundException e) {
            throw new MissingFileException("File " + ACCOUNTS_PATH + " is missing");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void loadLogs(Storage storage) {
        Path of = Path.of(LOGS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String username = tokens[IndexesInCommands.LOAD_LOGS_USERNAME_INDEX];
                String log = line.substring(username.length() + 1);
                storage.getAccount(username).addLog(log);
            }
        } catch (FileNotFoundException e) {
            throw new MissingFileException("File " + LOGS_PATH + " is missing");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void saveAccount(Account account) {
        try (var bufferedWriter = Files.newBufferedWriter(Path.of(ACCOUNTS_PATH), APPEND)) {
            String accountInfo =
                account.firstName() + " " + account.lastName() + " " + account.username() + " " + account.password();
            bufferedWriter.write(accountInfo);
            bufferedWriter.write(System.lineSeparator());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }
    }

    public void saveLog(String username, String log) {

        Path of = Path.of(LOGS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        try (var bufferedWriter = Files.newBufferedWriter(of, APPEND)) {
            String logInfo = username + " " + log;
            bufferedWriter.write(logInfo);
            bufferedWriter.write(System.lineSeparator());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }
    }

    public void saveFriend(String username, String friendUsername, double payedByOwner, double payedByFriend) {
        try (var bufferedWriter = Files.newBufferedWriter(Path.of(FRIENDS_PATH), APPEND)) {
            String friendInfo = username + " " + friendUsername + " " + payedByOwner + " " + payedByFriend;
            bufferedWriter.write(friendInfo);
            bufferedWriter.write(System.lineSeparator());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }
    }

    public void saveGroup(String groupName, String ownerUsername, double payedByOwner,
                          Map<String, Double> payedByGroup) {
        try (var bufferedWriter = Files.newBufferedWriter(Path.of(GROUPS_PATH), APPEND)) {
            StringBuilder groupInfo = new StringBuilder(groupName + " " + ownerUsername + " " + payedByOwner);
            for (Map.Entry<String, Double> entry : payedByGroup.entrySet()) {
                groupInfo.append(" ").append(entry.getKey()).append(" ").append(entry.getValue());
            }
            bufferedWriter.write(groupInfo.toString());
            bufferedWriter.write(System.lineSeparator());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }
    }

    public void updateGroup(String groupName, String ownerUsername, double payedByOwner,
                            Map<String, Double> payedByGroup) {
        StringBuilder sb = new StringBuilder();

        Path of = Path.of(GROUPS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String dbGroupName = tokens[IndexesInCommands.LOAD_GROUPS_GROUP_NAME_INDEX];
                String dbOwnerUsername = tokens[IndexesInCommands.LOAD_GROUPS_OWNER_USERNAME_INDEX];

                if (dbGroupName.equals(groupName) && dbOwnerUsername.equals(ownerUsername)) {
                    sb.append(groupName).append(" ").append(ownerUsername).append(" ").append(payedByOwner);
                    for (Map.Entry<String, Double> entry : payedByGroup.entrySet()) {
                        sb.append(" ").append(entry.getKey()).append(" ").append(entry.getValue());
                    }
                } else {
                    sb.append(line);
                }
                sb.append(System.lineSeparator());
            }
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }

        try (var bufferedWriter = Files.newBufferedWriter(of)) {
            bufferedWriter.write(sb.toString());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }

    }

    public void updateFriend(String usernameOwner, String friendUsername, double payedByOwner, double payedByFriend) {
        StringBuilder sb = new StringBuilder();

        Path of = Path.of(FRIENDS_PATH);
        if (!Files.exists(of)) {
            try {
                Files.createFile(of);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        try (var bufferedReader = Files.newBufferedReader(of)) {
            String line;

            while ((line = bufferedReader.readLine()) != null) {
                String[] tokens = line.split(" ");

                String dbUsernameOwner = tokens[IndexesInCommands.LOAD_FRIENDS_USERNAME_INDEX];
                String dbFriendUsername = tokens[IndexesInCommands.LOAD_FRIENDS_FRIEND_USERNAME_INDEX];

                if (dbUsernameOwner.equals(usernameOwner) && dbFriendUsername.equals(friendUsername)) {
                    sb.append(usernameOwner).append(" ").append(friendUsername).append(" ").append(payedByOwner)
                        .append(" ").append(payedByFriend);
                } else {
                    sb.append(line);
                }
                sb.append(System.lineSeparator());
            }
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }

        try (var bufferedWriter = Files.newBufferedWriter(of)) {
            bufferedWriter.write(sb.toString());
        } catch (IOException e) {
            throw new UpdateDataBaseException("Could not update database");
        }

    }
}

