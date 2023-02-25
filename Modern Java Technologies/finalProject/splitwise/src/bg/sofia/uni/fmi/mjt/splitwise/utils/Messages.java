package bg.sofia.uni.fmi.mjt.splitwise.utils;

public class Messages {
    public static final String INVALID_ARGS_COUNT_MESSAGE_FORMAT =
        "Invalid count of arguments: \"%s\" expects %d arguments. Example: \"%s\"";

    public static final String INVALID_COMMAND_MESSAGE = "Unknown command: %s" + System.lineSeparator() +
        "Type \"help\" for more information";

    public static final String USER_ALREADY_EXISTS = "User %s already exists";

    public static final String INVALID_PASSWORD =
        "Password should contain at least 8 characters at least one digit and at least one letter";

    public static final String REGISTERED_SUCCESSFULLY = "%s was registered successfully";

    public static final String WRONG_USERNAME_OR_PASSWORD = "Incorrect username or password";

    public static final String LOGIN_SUCCESSFULLY = "Logged in successfully";

    public static final String LOGGED_OUT_SUCCESSFULLY = "Logged out successfully";

    public static final String NOT_LOGGED_IN = "Perhaps you might want to log in first." + System.lineSeparator() +
        "Type \"help\" for more information.";

    public static final String USER_DOES_NOT_EXIST = "User %s does not exist";

    public static final String ALREADY_FRIENDS = "User %s is already your friend";

    public static final String ADDED_FRIEND_SUCCESSFULLY = "%s was added successfully to your friends";

    public static final String NOT_PERMITTED_OPERATION = "You are not permitted to perform this operation";

    public static final String NOT_FRIENDS = "%s is not your friend";

    public static final String INVALID_AMOUNT_PROVIDED = "Invalid amount provided only numbers are allowed";

    public static final String CANNOT_ADD_YOURSELF = "You cannot add yourself as a friend";

    public static final String CURRENT_STATUS = "Current status: %s";

    public static final String PAYED_SUCCESSFULLY = "%s %s (%s) payed you %.2f %s";

    public static final String SPLIT_SUCCESSFULLY = "Split %.2f %s between you and %s %s";

    public static final String GROUP_ALREADY_EXISTS = "Group %s already exists";

    public static final String NO_FRIENDS_IN_COMMAND_ADD_GROUP = "You have no friends to add to the group";

    public static final String GROUP_CREATED_SUCCESSFULLY = "Group %s was created successfully";

    public static final String GROUP_DOES_NOT_EXIST = "Group %s does not exist";

    public static final String SPLIT_SUCCESSFULLY_GROUP = "Split %.2f between you and %s group members";

    public static final String USER_DOES_NOT_EXIST_IN_GROUP = "User %s does not exist in group %s";

    public static final String PAYED_SUCCESSFULLY_GROUP = "%s %s (%s) payed you %.2f in group %s";

    public static final String LOGS_SUCCESSFULLY_SAVED = "Logs successfully saved in %s";

    public static final String LOGS_NOT_SAVED = "there was a problem saving the logs";

    public static final String HELP_LOGGED_OUT = "Available commands:" + System.lineSeparator() +
        "register <first name> <last name> <username> <password>" + System.lineSeparator() +
        "login <username> <password>" + System.lineSeparator() +
        "help" + System.lineSeparator() +
        "exit" + System.lineSeparator();

    public static final String HELP_LOGGED_IN = "Available commands:" + System.lineSeparator() +
        "logout" + System.lineSeparator() +
        "add-friend <username>" + System.lineSeparator() +
        "payed <username> <amount>" + System.lineSeparator() +
        "split <username> <amount> <reason>" + System.lineSeparator() +
        "create-group <name> <username1> <username2> ..." + System.lineSeparator() +
        "split-group <name> <amount> <reason>" + System.lineSeparator() +
        "payed-group <name> <username> <amount>" + System.lineSeparator() +
        "get-status" + System.lineSeparator() +
        "get-logs" + System.lineSeparator() +
        "help" + System.lineSeparator() +
        "exit" + System.lineSeparator();

    public static final String SPLIT_MONEY_MESSAGE = "You split %.2f BGN with %s - [%s]";

    public static final String SPLIT_MONEY_MESSAGE_GROUP_OWNER = "You split %.2f BGN with %s group - [%s]";
    public static final String SPLIT_MONEY_MESSAGE_GROUP_MEMBER = "%.2f BGN split with you from %s group - [%s]";
    public static final String SPLIT_MONEY_MESSAGE_FRIEND = "%s split %.2f BGN with you - [%s]";
    public static final String PAYED_MONEY_MESSAGE_FRIEND = "You payed %.2f BGN to %s";
    public static final String PAYED_MONEY_MESSAGE = "You registered %.2f payment from %s";
    public static final String SETTLED_MESSAGE = "settled up";
    public static final String YOU_OWE_MONEY_MESSAGE = "owes you %.2f BGN";
    public static final String YOU_ARE_OWED_MONEY_MESSAGE = "you owe %.2f BGN";
    public static final String FRIEND_IN_GROUP_MESSAGE = "%s - %s";
    public static final String PAYED_MONEY_MESSAGE_GROUP = "You registered %.2f BGN payment from %s in group %s";
    public static final String PAYED_MONEY_MESSAGE_FRIEND_GROUP = "You payed %.2f BGN to %s in group %s";


}
