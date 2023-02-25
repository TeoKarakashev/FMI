package bg.sofia.uni.fmi.mjt.splitwise.command.executors.group;

import bg.sofia.uni.fmi.mjt.splitwise.command.CommandCreator;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.InMemoryStorage;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;
import bg.sofia.uni.fmi.mjt.splitwise.utils.CommandConstants;
import bg.sofia.uni.fmi.mjt.splitwise.utils.IndexesInCommands;
import bg.sofia.uni.fmi.mjt.splitwise.utils.Messages;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.nio.channels.SelectionKey;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class GroupExecutorAPITest {

    private Storage storage;
    private CommandExecutor commandExecutor;

    private static final String registerUser1 = "register user userov user abc123456";
    private static final String registerUser2 = "register user userov user4o abc123456";
    private static final String registerUser3 = "register user userov user44o abc123456";
    private static final String loginUser1 = "login user abc123456";
    private static final String loginUser2 = "login user4o abc123456";

    @Mock
    private SelectionKey key;

    @BeforeEach
    public void setUp() {
        storage = new InMemoryStorage();
        commandExecutor = new CommandExecutorAPI(storage);

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
    public void testCreateGroupShouldWorkCorrectly() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        String expected = String.format(Messages.GROUP_CREATED_SUCCESSFULLY, "group1");
        String actual = commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        assertEquals(expected, actual, "Create group should work correctly");
    }

    @Test
    public void testCreateGroupShouldReturnAMessageForInvalidNumberOfParameters() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.CREATE_GROUP,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_CREATE_GROUP_COMMAND,
            CommandConstants.CREATE_GROUP + " <group_name> <username1> <username2> ... <usernameN>");
        String actual = commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        assertEquals(expected, actual, "Create group should return a message for invalid number of parameters");
    }

    @Test
    public void testCreateGroupShouldWorkOnlyForUniqueNames() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String expected = String.format(Messages.GROUP_ALREADY_EXISTS, "group1");
        String actual = commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        assertEquals(expected, actual, "Create group should work only for unique names");
    }

    @Test
    public void testCreateGroupCannotAddYourselfInTheGroup() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String expected = Messages.CANNOT_ADD_YOURSELF;
        String actual = commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        assertEquals(expected, actual, "Create group cannot add yourself in the group");
    }

    @Test
    public void testCreateGroupCannotAddNonExistingAccount() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user4400";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String expected = String.format(Messages.USER_DOES_NOT_EXIST, "user4400");
        String actual = commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        assertEquals(expected, actual, "Create group cannot add non existing account");
    }

    @Test
    public void testPayedGroupShouldWorkCorrectly() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "payed-group group1 user4o 100";
        Account accountToPay = storage.getAccount("user4o");
        double amount = 100;
        String expected =
            String.format(Messages.PAYED_SUCCESSFULLY_GROUP, accountToPay.firstName(), accountToPay.lastName(),
                accountToPay.username(), amount, "group1") + System.lineSeparator() +
                "Current status: user4o - you owe 100,00 BGN" + System.lineSeparator() +
                "user44o - settled up" + System.lineSeparator();
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should work correctly");
    }

    @Test
    public void testPayedGroupShouldReturnAMessageIfInvalidAmountOfArguments() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "payed-group group1 user4o";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.PAYED_GROUP,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_PAYED_GROUP_COMMAND,
            CommandConstants.PAYED_GROUP + " <group name> <username> <amount>");
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should return a message if invalid amount of arguments");
    }

    @Test
    public void testPayedGroupShouldReturnAMessageIfAmountIsInvalid() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "payed-group group1 stoPari4ki user4o";
        String expected = Messages.INVALID_AMOUNT_PROVIDED;
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should return a message if amount is invalid");
    }

    @Test
    public void testPayedGroupShouldReturnAMessageIfGroupDoesntExist() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "payed-group group2 user4o 100";
        String expected = String.format(Messages.GROUP_DOES_NOT_EXIST, "group2");
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should return a message if group doesn't exist");
    }

    @Test
    public void testPayedGroupShouldReturnAMessageIfMemberIsNotInTheGroup() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "payed-group group1 user440)o 100";
        String expected = String.format(Messages.USER_DOES_NOT_EXIST_IN_GROUP, "user440)o", "group1");
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should return a message if member is not in the group");
    }

    @Test
    public void testSplitGroupShouldSplitMoneyEquallyBetweenEveryoneInTheGroup() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "split-group group1 150 test reason";
        double amount = 150;
        String expected =
            String.format(Messages.SPLIT_SUCCESSFULLY_GROUP, amount, "group1") + System.lineSeparator() +
                "Current status: user4o - owes you 50,00 BGN" + System.lineSeparator() +
                "user44o - owes you 50,00 BGN" + System.lineSeparator();
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should work correctly");
    }

    @Test
    public void testSplitGroupShouldReturnAMessageForInvalidNumberOfArguments() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "split-group group1 reason";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.SPLIT_GROUP,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_SPLIT_GROUP_COMMAND,
            CommandConstants.SPLIT_GROUP + " <group-name> <amount> <reason>");
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should work correctly");
    }

    @Test
    public void testSplitGroupShouldReturnAMessageForInvalidAmount() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser3), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String createGroup = "create-group group1 user4o user44o";
        commandExecutor.execute(CommandCreator.newCommand(createGroup), key);
        String payedGroup = "split-group group1 gogo reason";
        String expected = Messages.INVALID_AMOUNT_PROVIDED;
        String actual = commandExecutor.execute(CommandCreator.newCommand(payedGroup), key);
        assertEquals(expected, actual, "Payed group should work correctly");
    }


}