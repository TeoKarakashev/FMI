package bg.sofia.uni.fmi.mjt.splitwise.storage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.FRIEND_IN_GROUP_MESSAGE;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.PAYED_MONEY_MESSAGE;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.PAYED_MONEY_MESSAGE_FRIEND;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.PAYED_MONEY_MESSAGE_FRIEND_GROUP;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.PAYED_MONEY_MESSAGE_GROUP;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.SETTLED_MESSAGE;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.SPLIT_MONEY_MESSAGE;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.SPLIT_MONEY_MESSAGE_FRIEND;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.SPLIT_MONEY_MESSAGE_GROUP_MEMBER;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.SPLIT_MONEY_MESSAGE_GROUP_OWNER;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.YOU_ARE_OWED_MONEY_MESSAGE;
import static bg.sofia.uni.fmi.mjt.splitwise.utils.Messages.YOU_OWE_MONEY_MESSAGE;

public record Account(String firstName, String lastName, String username, String password,
                      Map<String, FriendConnection> ownerToFriendConnection,
                      Map<String, FriendConnection> memberOfFriendConnection,
                      Map<String, GroupConnection> ownerToAGroup, Map<String, GroupConnection> memberOfAGroup,
                      List<String> logs) {

    public Account(String firstName, String lastName, String username, String password) {
        this(firstName, lastName, username, password, new HashMap<>(), new HashMap<>(), new HashMap<>(),
            new HashMap<>(),
            new ArrayList<>());
    }

    public void createGroup(Set<Account> groupMembers, String groupName) {
        createGroup(groupMembers, groupName, 0, new HashMap<>());

    }

    public void createGroup(Set<Account> groupMembers, String groupName, double payedByOwner,
                            Map<String, Double> payedByGroup) {
        GroupConnection groupConnection = new GroupConnection(this.username, payedByOwner, payedByGroup);
        if (payedByGroup.isEmpty()) {
            groupMembers.forEach(member -> {
                groupConnection.addMember(member.username);
                member.memberOfAGroup.put(groupName, groupConnection);
            });
        } else {
            groupMembers.forEach(member -> {
                member.memberOfAGroup.put(groupName, groupConnection);
            });
        }
        ownerToAGroup.put(groupName, groupConnection);
    }

    public void payGroup(Account friend, GroupConnection groupConnection, double amount, String groupName,
                         LocalDataBaseAPI dataBaseAPI) {
        groupConnection.payed(friend.username, amount);
        dataBaseAPI.updateGroup(groupName, groupConnection.owner(), groupConnection.payedByOwner(),
            groupConnection.payedByGroup());

        String logOwner = String.format(PAYED_MONEY_MESSAGE_GROUP, amount, friend.username, groupName);
        String logFriend = String.format(PAYED_MONEY_MESSAGE_FRIEND_GROUP, amount, this.username, groupName);

        addLog(logOwner);
        friend.addLog(logFriend);

        dataBaseAPI.saveLog(this.username, logOwner);
        dataBaseAPI.saveLog(friend.username, logFriend);
    }

    public GroupConnection getGroupConnection(String group) {
        return ownerToAGroup.get(group);
    }

    public void splitGroup(String groupName, GroupConnection groupConnection, double amount, List<String> reason,
                           Storage storage, LocalDataBaseAPI dataBaseAPI) {
        String reasonString = String.join(" ", reason);
        groupConnection.split(amount);
        dataBaseAPI.updateGroup(groupName, groupConnection.owner(), groupConnection.payedByOwner(),
            groupConnection.payedByGroup());

        String log = String.format(SPLIT_MONEY_MESSAGE_GROUP_OWNER, amount, groupName, reasonString);
        addLog(log);
        dataBaseAPI.saveLog(this.username, log);
        groupConnection.memberOfGroup().forEach((member, value) -> {
            Account memberAccount = storage.getAccount(member);
            if (memberAccount != null) {
                String logMember = String.format(SPLIT_MONEY_MESSAGE_GROUP_MEMBER, amount, groupName, reasonString);
                memberAccount.addLog(logMember);
                dataBaseAPI.saveLog(member, logMember);
            }
        });
    }

    public void addFriend(Account friend, double payedByOwner, double payedByFriend) {
        FriendConnection friendConnection = new FriendConnection(payedByOwner, payedByFriend);
        ownerToFriendConnection.put(friend.username, friendConnection);
        friend.memberOfFriendConnection.put(this.username, friendConnection);
    }

    public void addFriend(Account friend) {
        addFriend(friend, 0, 0);
    }

    public void addLog(String log) {
        logs.add(log);
    }

    public String getStatus(String username, boolean isOwner) {
        FriendConnection friendConnection;
        if (isOwner) {
            friendConnection = ownerToFriendConnection.get(username);
            if (friendConnection.payedByOwner() == friendConnection.payedByFriend()) {
                return SETTLED_MESSAGE;
            } else if (friendConnection.payedByOwner() > friendConnection.payedByFriend()) {
                return String.format(YOU_OWE_MONEY_MESSAGE,
                    Math.abs(friendConnection.payedByOwner() - friendConnection.payedByFriend()));
            } else {
                return String.format(YOU_ARE_OWED_MONEY_MESSAGE,
                    Math.abs(friendConnection.payedByFriend() - friendConnection.payedByOwner()));
            }
        } else {
            friendConnection = memberOfFriendConnection.get(username);
            if (friendConnection.payedByOwner() == friendConnection.payedByFriend()) {
                return SETTLED_MESSAGE;
            } else if (friendConnection.payedByOwner() < friendConnection.payedByFriend()) {
                return String.format(YOU_OWE_MONEY_MESSAGE,
                    Math.abs(friendConnection.payedByOwner() - friendConnection.payedByFriend()));
            } else {
                return String.format(YOU_ARE_OWED_MONEY_MESSAGE,
                    Math.abs(friendConnection.payedByFriend() - friendConnection.payedByOwner()));
            }
        }
    }

    public String getGroupStatus(GroupConnection groupConnection, boolean isOwner) {
        StringBuilder sb = new StringBuilder();
        if (isOwner) {
            groupConnection.payedByGroup().forEach((username, amount) -> {
                if (amount == groupConnection.payedByOwner()) {
                    sb.append(String.format(FRIEND_IN_GROUP_MESSAGE, username, SETTLED_MESSAGE));
                } else if (amount > groupConnection.payedByOwner()) {
                    sb.append(String.format(FRIEND_IN_GROUP_MESSAGE, username, String.format(YOU_ARE_OWED_MONEY_MESSAGE,
                        Math.abs(amount - groupConnection.payedByOwner()))));
                } else {
                    sb.append(String.format(FRIEND_IN_GROUP_MESSAGE, username,
                        String.format(YOU_OWE_MONEY_MESSAGE, Math.abs(groupConnection.payedByOwner() - amount))));
                }
                sb.append(System.lineSeparator());
            });

        } else {
            Double amount = groupConnection.payedByGroup().get(this.username);

            sb.append(groupConnection.owner()).append(" - ");
            if (amount == groupConnection.payedByOwner()) {
                sb.append(SETTLED_MESSAGE);
            } else if (amount < groupConnection.payedByOwner()) {
                sb.append(String.format(YOU_ARE_OWED_MONEY_MESSAGE, Math.abs(groupConnection.payedByOwner() - amount)));
            } else {
                sb.append(String.format(YOU_OWE_MONEY_MESSAGE, Math.abs(amount - groupConnection.payedByOwner())));
            }
            sb.append(System.lineSeparator());
        }
        return sb.toString();
    }

    public void split(Account friend, Double amount, List<String> reason, LocalDataBaseAPI dataBaseAPI) {
        FriendConnection friendConnection = ownerToFriendConnection.get(friend.username);
        friendConnection.split(amount);
        dataBaseAPI.updateFriend(this.username, friend.username, friendConnection.payedByOwner(),
            friendConnection.payedByFriend());
        String reasonString = String.join(" ", reason);
        String log = String.format(SPLIT_MONEY_MESSAGE, amount, friend.username, reasonString);
        addLog(log);
        dataBaseAPI.saveLog(this.username, log);
        String logFriend = String.format(SPLIT_MONEY_MESSAGE_FRIEND, this.username, amount, reasonString);
        friend.addLog(logFriend);
        dataBaseAPI.saveLog(friend.username, logFriend);
    }

    public void payed(Account friend, Double amount, LocalDataBaseAPI dataBaseAPI) {
        FriendConnection friendConnection = ownerToFriendConnection.get(friend.username);
        friendConnection.payed(amount);
        dataBaseAPI.updateFriend(this.username, friend.username, friendConnection.payedByOwner(),
            friendConnection.payedByFriend());
        String log = String.format(PAYED_MONEY_MESSAGE, amount, friend.username);
        addLog(log);
        dataBaseAPI.saveLog(this.username, log);
        String logFriend = String.format(PAYED_MONEY_MESSAGE_FRIEND, amount, this.username);
        friend.addLog(logFriend);
        dataBaseAPI.saveLog(friend.username, logFriend);
    }

    public String status() {
        StringBuilder sb = new StringBuilder();
        if (ownerToFriendConnection.isEmpty() && memberOfFriendConnection.isEmpty()) {
            sb.append("No friends");
            sb.append(System.lineSeparator());
        } else {
            sb.append("Friends:");
            sb.append(System.lineSeparator());
            ownerToFriendConnection.forEach((username, friendConnection) -> {
                sb.append(username);
                sb.append(" - ");
                sb.append(getStatus(username, true));
                sb.append(System.lineSeparator());
            });
            memberOfFriendConnection.forEach((username, friendConnection) -> {
                if (!ownerToFriendConnection.containsKey(username)) {
                    sb.append(username);
                    sb.append(" - ");
                    sb.append(getStatus(username, false));
                    sb.append(System.lineSeparator());
                }
            });
        }
        if (ownerToAGroup.isEmpty() && memberOfAGroup.isEmpty()) {
            sb.append("No groups");
        } else {
            sb.append("Groups:");
            sb.append(System.lineSeparator());
            ownerToAGroup.forEach((groupName, groupConnection) -> {
                sb.append(groupName);
                sb.append(System.lineSeparator());
                sb.append(getGroupStatus(groupConnection, true));
            });
            memberOfAGroup.forEach((groupName, groupConnection) -> {
                if (!ownerToAGroup.containsKey(groupName)) {
                    sb.append(groupName);
                    sb.append(System.lineSeparator());
                    sb.append(getGroupStatus(groupConnection, false));
                }
            });
        }
        return sb.toString();
    }
}
