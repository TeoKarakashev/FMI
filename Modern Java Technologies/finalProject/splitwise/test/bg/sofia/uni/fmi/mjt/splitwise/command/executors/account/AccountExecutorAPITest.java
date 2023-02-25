package bg.sofia.uni.fmi.mjt.splitwise.command.executors.account;

import bg.sofia.uni.fmi.mjt.splitwise.command.CommandCreator;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutorAPI;
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
public class AccountExecutorAPITest {

    private Storage storage;
    private CommandExecutor commandExecutor;
    private LocalDataBaseAPI dataBaseAPI;

    private static final String registerUser1 = "register user userov user abc123456";
    private static final String loginUser1 = "login user abc123456";

    @Mock
    private SelectionKey key;

    @BeforeEach
    public void setUp() {
        storage = new InMemoryStorage();
        dataBaseAPI = new LocalDataBaseAPI();
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
    public void testRegisterUserShouldWorkCorrectly() {
        String expected = String.format(Messages.REGISTERED_SUCCESSFULLY, "user");
        String actual = commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        assertEquals(expected, actual, "Register user should register the user");
    }

    @Test
    public void testRegisterUserShouldWorkCorrectlyWithUsernameAlreadyInStorage() {
        String expected = String.format(Messages.USER_ALREADY_EXISTS, "user");
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        String actual = commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);
        assertEquals(expected, actual, "Register user should work correctly with username already in storage");
    }

    @Test
    public void testRegisterUserWithInvalidPassword() {
        String command = "register user userov user 123";
        String expected = Messages.INVALID_PASSWORD;
        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Register user should work correctly with invalid password");
    }

    @Test
    public void testRegisterUserWithInvalidNumberOfParameters() {
        String command = "register user user 123";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.REGISTER,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_REGISTER_COMMAND,
            CommandConstants.REGISTER + " <first name> <last name> <username> <password>");


        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);
        assertEquals(expected, actual, "Register user should work correctly with invalid number of parameters");
    }

    @Test
    public void testLoginShouldWorkCorrectlyWithCorrectUserInDB() {
        String expected = Messages.LOGIN_SUCCESSFULLY;
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(loginUser1), key);

        assertEquals(expected, actual, "Login should work correctly with correct user in DB");
    }

    @Test
    public void testLoginShouldWorkCorrectlyWithWrongPassword() {
        String login = "login user 12345678";
        String expected = Messages.WRONG_USERNAME_OR_PASSWORD;
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(login), key);

        assertEquals(expected, actual, "Login should work correctly with incorrect password");
    }

    @Test
    public void testLoginShouldWorkCorrectlyWithWrongUsername() {
        String login = "login user1 12345678a";
        String expected = Messages.WRONG_USERNAME_OR_PASSWORD;
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(login), key);

        assertEquals(expected, actual, "Login should work correctly with incorrect username");
    }

    @Test
    public void testLoginShouldWorkCorrectlyWithInvalidParameters() {
        String login = "login user1 12345678a test21131 asdasd";
        String expected = String.format(Messages.INVALID_ARGS_COUNT_MESSAGE_FORMAT, CommandConstants.LOGIN,
            IndexesInCommands.ARGUMENTS_COUNT_FOR_LOGIN_COMMAND,
            CommandConstants.LOGIN + " <username> <password>");
        commandExecutor.execute(CommandCreator.newCommand(registerUser1), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(login), key);

        assertEquals(expected, actual, "Login should work correctly with invalid parameters");
    }

    @Test
    public void testHelpWhenNotLoggedIn() {
        String command = "help";
        String expected = Messages.HELP_LOGGED_OUT;
        commandExecutor.execute(CommandCreator.newCommand(command), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);

        assertEquals(expected, actual, "Help should work correctly when not logged in");
    }

    @Test
    public void testInvalidCommandWhenNotLoggedIn() {
        String command = "helper4e";
        String expected = Messages.NOT_LOGGED_IN;
        commandExecutor.execute(CommandCreator.newCommand(command), key);

        String actual = commandExecutor.execute(CommandCreator.newCommand(command), key);

        assertEquals(expected, actual, "Invalid command should work correctly when not logged in");
    }

}