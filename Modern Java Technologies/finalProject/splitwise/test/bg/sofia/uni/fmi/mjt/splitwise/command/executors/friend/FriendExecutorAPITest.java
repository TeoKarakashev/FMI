package bg.sofia.uni.fmi.mjt.splitwise.command.executors.friend;

import bg.sofia.uni.fmi.mjt.splitwise.command.CommandCreator;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Account;
import bg.sofia.uni.fmi.mjt.splitwise.storage.InMemoryStorage;
import bg.sofia.uni.fmi.mjt.splitwise.storage.LocalDataBaseAPI;
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
public class FriendExecutorAPITest {

    private Storage storage;
    private CommandExecutor commandExecutor;

    private static final String registerUser1 = "register user userov user abc123456";
    private static final String registerUser2 = "register user40 userov user4o abc123456";
    private static final String loginUser1 = "login user abc123456";

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
    public void testAddFriendShouldWorkCorrectly() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user4o";
        String expected = String.format(Messages.ADDED_FRIEND_SUCCESSFULLY, "user4o");
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Add friend should work correctly");
    }

    @Test
    public void testAddFriendShouldCannotAddSameUserAsFriendTwice() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user4o";
        commandExecutor.execute(CommandCreator.newCommand(command), key);
        String expected = String.format(Messages.ALREADY_FRIENDS, "user4o");
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Cannot add same user as friend twice");
    }

    @Test
    public void testAddFriendShouldCannotAddUserThatDoesntExist() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user40";
        String expected = String.format(Messages.USER_DOES_NOT_EXIST, "user40");
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Cannot add same user as friend twice");
    }

    @Test
    public void testAddFriendShouldCannotAddYouSelf() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user";
        String expected = String.format(Messages.CANNOT_ADD_YOURSELF);
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Cannot add yourself");
    }

    @Test
    public void testAddFriendShouldWorkCorrectlyWithInvalidParameters() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user user2";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.ADD_FRIEND,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_ADD_FRIEND_COMMAND,
            CommandConstants.ADD_FRIEND + " <username>");
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Add friend should work correctly with invalid parameters");
    }

    @Test
    public void testSplitWithFriendShouldWorkCorrectly() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user4o";
        commandExecutor.execute(CommandCreator.newCommand(command), key);
        command = "split user4o 1000 test reason";
        String expected = "Split 1000,00 BGN between you and user40 userov" + System.lineSeparator() +
            "Current status: owes you 500,00 BGN";
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Split with friend should work correctly");
    }

    @Test
    public void testPayedFriendShouldWorkCorrectly() {
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        commandExecutor.execute(CommandCreator.newCommand(registerUser2), key);
        commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);
        String command = "add-friend user4o";
        commandExecutor.execute(CommandCreator.newCommand(command), key);
        command = "payed user4o 500";
        String expected = "user40 userov (user4o) payed you 500,00 BGN" + System.lineSeparator() +
            "Current status: you owe 500,00 BGN";
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Split with friend should work correctly");
    }
}
