package bg.sofia.uni.fmi.mjt.splitwise.storage;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class AccountTest {

    @BeforeEach
    public void setUp() {
        File accountsFile = new File("accounts.txt");
        File groupsFile = new File("groups.txt");
        File friendsFile = new File("friends.txt");
        File logsFile = new File("logs.txt");
    }

    @AfterEach
    public void tearDown() {
        File accountsFile = new File(Path.of("accounts.txt").toAbsolutePath().toString());
        File groupsFile = new File(Path.of("groups.txt").toAbsolutePath().toString());
        File friendsFile = new File(Path.of("friends.txt").toAbsolutePath().toString());
        File logsFile = new File(Path.of("logs.txt").toAbsolutePath().toString());
        accountsFile.delete();
        groupsFile.delete();
        friendsFile.delete();
        logsFile.delete();
    }

    @Test
    public void testCreateGroupThatHasntExsistedBeforeShouldWorkCorrectly() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Account account3 = new Account("bobo", "bobov", "bobo", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        members.add(account3);

        account.createGroup(members, "group1");

        assertEquals(1, account.ownerToAGroup().size(), "createGroup should add the group to the ownerToAGroup map");
        assertEquals(1, account2.memberOfAGroup().size(), "createGroup should add the group to the memberOfAGroup map");
        assertEquals(1, account3.memberOfAGroup().size(), "createGroup should add the group to the memberOfAGroup map");
        assertEquals(0, account.ownerToAGroup().get("group1").payedByOwner(),
            "createGroup should set the payedByOwner to 0");
    }

    @Test
    public void testCreateGroupThatIsLoadedFromTheDataBaseShouldWorkCorrectly() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Account account3 = new Account("bobo", "bobov", "bobo", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        members.add(account3);
        String groupName = "group1";
        double payedByOwner = 10;
        Map<String, Double> payedByGroupMembers = Map.of("pesho", 5.0, "bobo", 7.0);
        account.createGroup(members, groupName, payedByOwner, payedByGroupMembers);

        assertEquals(1, account.ownerToAGroup().size(), "createGroup should add the group to the ownerToAGroup map");
        assertEquals(1, account2.memberOfAGroup().size(), "createGroup should add the group to the memberOfAGroup map");
        assertEquals(1, account3.memberOfAGroup().size(), "createGroup should add the group to the memberOfAGroup map");
        assertEquals(payedByOwner, account.ownerToAGroup().get(groupName).payedByOwner(),
            "createGroup should set the payedByOwner to 0");
        assertEquals(7, payedByGroupMembers.get("bobo"), "createGroup should set the payedByGroupMembers correctly");
        assertEquals(5, payedByGroupMembers.get("pesho"), "createGroup should set the payedByGroupMembers correctly");
    }

    @Test
    public void testPayGroupShouldUpdateMemberPayedMoney() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Account account3 = new Account("bobo", "bobov", "bobo", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        members.add(account3);
        String groupName = "group1";
        double payedByOwner = 10;
        Map<String, Double> payedByGroupMembers = new HashMap<>();
        payedByGroupMembers.put("pesho", 5.0);
        payedByGroupMembers.put("bobo", 7.0);
        account.createGroup(members, groupName, payedByOwner, payedByGroupMembers);
        GroupConnection groupConnection = account.ownerToAGroup().get(groupName);
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        assertEquals(5, payedByGroupMembers.get("pesho"), "payed by member is correct before payGroup");
        account.payGroup(account2, groupConnection, 5, groupName, localDataBaseAPI);
        assertEquals(10, payedByGroupMembers.get("pesho"), "payed by member is correct after payGroup");

    }

    @Test
    public void testSplitGroupShouldUpdateOwnerMoneyCorrectly() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Account account3 = new Account("bobo", "bobov", "bobo", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        members.add(account3);

        account.createGroup(members, "group1");

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        Storage storage = new InMemoryStorage();
        storage.addAccount(account);
        storage.addAccount(account2);
        storage.addAccount(account3);
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        List<String> reasons = new ArrayList<>();
        reasons.add("test");
        reasons.add("for");
        reasons.add("splitGroup");

        account.splitGroup("group1", groupConnection, 15, reasons, storage, localDataBaseAPI);
        assertEquals(5, account.ownerToAGroup().get("group1").payedByOwner(),
            "splitGroup should update the payedByOwner correctly according to size of group");

    }

    @Test
    public void testAddFriendFromDataBaseShouldWorkCorrectly() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 12.50;
        double amount2 = 5;
        account.addFriend(account2, amount, amount2);

        assertEquals(1, account.ownerToFriendConnection().size(), "addFriend should add the friend to the friends map");
        assertEquals(1, account2.memberOfFriendConnection().size(),
            "addFriend should add the friend to the friends map");
        assertEquals(amount, account.ownerToFriendConnection().get(account2.username()).payedByOwner(),
            "addFriend should set the amount correctly");
        assertEquals(amount2, account.ownerToFriendConnection().get(account2.username()).payedByFriend(),
            "addFriend should set the amount correctly");
        assertEquals(amount, account2.memberOfFriendConnection().get(account.username()).payedByOwner(),
            "addFriend should set the amount correctly");
        assertEquals(amount2, account2.memberOfFriendConnection().get(account.username()).payedByFriend(),
            "addFriend should set the amount correctly");

    }

    @Test
    public void testAddNewFriendShouldWorkCorrectly() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        account.addFriend(account2);
        assertEquals(1, account.ownerToFriendConnection().size(), "addFriend should add the friend to the friends map");
        assertEquals(1, account2.memberOfFriendConnection().size(),
            "addFriend should add the friend to the friends map");
        assertEquals(0, account.ownerToFriendConnection().get(account2.username()).payedByOwner(),
            "addFriend should set the amount correctly");
        assertEquals(0, account.ownerToFriendConnection().get(account2.username()).payedByFriend(),
            "addFriend should set the amount correctly");
        assertEquals(0, account2.memberOfFriendConnection().get(account.username()).payedByOwner(),
            "addFriend should set the amount correctly");
        assertEquals(0, account2.memberOfFriendConnection().get(account.username()).payedByFriend(),
            "addFriend should set the amount correctly");
    }

    @Test
    public void testGetStatusForFriendWhileOwnerConnectionSettledUp() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        double amount2 = 10;
        account.addFriend(account2, amount, amount2);
        assertEquals("settled up", account.getStatus(account2.username(), true),
            "getStatus should return settled up");
    }

    @Test
    public void testGetStatusForFriendWhileMemberConnectionSettledUp() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        double amount2 = 10;
        account.addFriend(account2, amount, amount2);
        assertEquals("settled up", account2.getStatus(account.username(), false),
            "getStatus should return settled up");
    }

    @Test
    public void testGetStatusForFriendWhileOwnerConnectionYouAreOwedMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 15;
        double amount2 = 10;
        account.addFriend(account2, amount, amount2);
        assertEquals("owes you 5,00 BGN", account.getStatus(account2.username(), true),
            "getStatus should return settled up");
    }

    @Test
    public void testGetStatusForFriendWhileOwnerConnectionYouOweMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        double amount2 = 15;
        account.addFriend(account2, amount, amount2);
        assertEquals("you owe 5,00 BGN", account.getStatus(account2.username(), true),
            "getStatus should return settled up");
    }

    @Test
    public void testGetStatusForFriendWhileMemberConnectionYouAreOwedMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        double amount2 = 15;
        account.addFriend(account2, amount, amount2);
        assertEquals("owes you 5,00 BGN", account2.getStatus(account.username(), false),
            "getStatus should return settled up");
    }

    @Test
    public void testGetStatusForFriendWhileMemberConnectionYouOweMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 15;
        double amount2 = 10;
        account.addFriend(account2, amount, amount2);
        assertEquals("you owe 5,00 BGN", account2.getStatus(account.username(), false),
            "getStatus should return settled up");
    }


    @Test
    public void testGetGroupStatusForOwnerShouldWorkCorrectlyWithOweMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 10;
        double amount2 = 15;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "pesho - you owe 5,00 BGN" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account.getGroupStatus(groupConnection, true),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testGetGroupStatusForOwnerShouldWorkCorrectlyWithSettledUp() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 10;
        double amount2 = 10;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "pesho - settled up" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account.getGroupStatus(groupConnection, true),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testGetGroupStatusForOwnerShouldWorkCorrectlyWithBeingOwedMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 10;
        double amount2 = 5;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "pesho - owes you 5,00 BGN" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account.getGroupStatus(groupConnection, true),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testGetGroupStatusForMemberShouldWorkCorrectlyWithBeingOwedMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 5;
        double amount2 = 10;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "gogo - owes you 5,00 BGN" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account2.getGroupStatus(groupConnection, false),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testGetGroupStatusForMemberShouldWorkCorrectlyWithOweMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 10;
        double amount2 = 5;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "gogo - you owe 5,00 BGN" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account2.getGroupStatus(groupConnection, false),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testGetGroupStatusForMemberShouldWorkCorrectlyWithSettledUp() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Set<Account> members = new HashSet<>();
        members.add(account2);

        double amount = 10;
        double amount2 = 10;

        Map<String, Double> payedByGroup = new HashMap<>();
        payedByGroup.put(account2.username(), amount2);
        account.createGroup(members, "group1", amount, payedByGroup);
        String expectedString =
            "gogo - settled up" + System.lineSeparator();

        GroupConnection groupConnection = account.ownerToAGroup().get("group1");

        assertEquals(expectedString, account2.getGroupStatus(groupConnection, false),
            "getGroupStatus should return the correct string");
    }

    @Test
    public void testSplitWithFriendShouldAddHalfOfTheAmountInOwner() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        List<String> reasons = new ArrayList<>();
        reasons.add("reason1");
        reasons.add("reason2");
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        account.addFriend(account2);
        account.split(account2, amount, reasons, localDataBaseAPI);
        assertEquals(5, account.ownerToFriendConnection().get("pesho").payedByOwner(),
            "splitWithFriend should add half of the amount in owner");
        assertEquals(0, account.ownerToFriendConnection().get("pesho").payedByFriend(),
            "splitWithFriend should add half of the amount in owner");
        assertEquals(5, account2.memberOfFriendConnection().get("gogo").payedByOwner(),
            "splitWithFriend should add half of the amount in owner");
        assertEquals(0, account2.memberOfFriendConnection().get("gogo").payedByFriend(),
            "splitWithFriend should add half of the amount in owner");

    }

    @Test
    public void testPayedForFriendShouldIncrementPayedByFriend() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        account.addFriend(account2);
        account.payed(account2, amount, localDataBaseAPI);
        assertEquals(0, account.ownerToFriendConnection().get("pesho").payedByOwner(),
            "payedForFriend should increment payedByFriend");
        assertEquals(10, account.ownerToFriendConnection().get("pesho").payedByFriend(),
            "payedForFriend should increment payedByFriend");

    }

    @Test
    public void testStatusWithNoFriendAndGroups() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        String expectedString = "No friends" + System.lineSeparator() + "No groups";
        assertEquals(expectedString, account.status(), "status should return the correct string");
    }

    @Test
    public void testStatusWithOnlyFriendsOweMoneyAndNoGroupsByOwner() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        account.addFriend(account2);
        account.payed(account2, amount, localDataBaseAPI);
        String expectedString =
            "Friends:" + System.lineSeparator() + "pesho - you owe 10,00 BGN" + System.lineSeparator() + "No groups";
        assertEquals(expectedString, account.status(), "status should return the correct string");

    }

    @Test
    public void testStatusWithOnlyFriendsOweMoneyAndNoGroupsByMember() {

        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        double amount = 10;
        LocalDataBaseAPI localDataBaseAPI = new LocalDataBaseAPI();
        account.addFriend(account2);
        account.payed(account2, amount, localDataBaseAPI);
        String expectedString =
            "Friends:" + System.lineSeparator() + "gogo - owes you 10,00 BGN" + System.lineSeparator() + "No groups";
        assertEquals(expectedString, account2.status(), "status should return the correct string");

    }

    @Test
    public void testStatusWithNoFriendsAndOwnerOfAGroupIsOwedMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        String groupName = "group1";
        double payedByOwner = 10;
        Map<String, Double> payedByGroupMembers = Map.of("pesho", 5.0);
        account.createGroup(members, groupName, payedByOwner, payedByGroupMembers);
        String expectedString =
            "No friends" + System.lineSeparator() + "Groups:" + System.lineSeparator() + "group1" +
                System.lineSeparator() +
                "pesho - owes you 5,00 BGN" + System.lineSeparator();
        assertEquals(expectedString, account.status(), "status should return the correct string");
    }

    @Test
    public void testStatusWithNoFriendsAndOwnerOfAGroupOwesMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        String groupName = "group1";
        double payedByOwner = 5;
        Map<String, Double> payedByGroupMembers = Map.of("pesho", 10.0);
        account.createGroup(members, groupName, payedByOwner, payedByGroupMembers);
        String expectedString =
            "No friends" + System.lineSeparator() + "Groups:" + System.lineSeparator() + "group1" +
                System.lineSeparator() +
                "pesho - you owe 5,00 BGN" + System.lineSeparator();
        assertEquals(expectedString, account.status(), "status should return the correct string");
    }

    @Test
    public void testStatusWithNoFriendsAndMemberOfAGroupOwesMoney() {
        Account account = new Account("gogo", "gogov", "gogo", "123");
        Account account2 = new Account("pesho", "peshov", "pesho", "123");
        Account account3 = new Account("ivan", "ivanov", "ivan", "123");

        Set<Account> members = new HashSet<>();
        members.add(account2);
        members.add(account3);
        String groupName = "group1";
        double payedByOwner = 5;
        Map<String, Double> payedByGroupMembers = Map.of("pesho", 10.0, "ivan", 5.0);
        account.createGroup(members, groupName, payedByOwner, payedByGroupMembers);
        String expectedString =
            "No friends" + System.lineSeparator() + "Groups:" + System.lineSeparator() + "group1" +
                System.lineSeparator() +
                "gogo - owes you 5,00 BGN" + System.lineSeparator();
        assertEquals(expectedString, account2.status(), "status should return the correct string");
    }


}
