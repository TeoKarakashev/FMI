package bg.sofia.uni.fmi.mjt.splitwise.utils;

public class IndexesInCommands {
    public static final int ARGUMENTS_COUNT_FOR_REGISTER_COMMAND = 4;
    public static final int FIRSTNAME_INDEX_IN_REGISTER = 0;
    public static final int LASTNAME_INDEX_IN_REGISTER = 1;
    public static final int USERNAME_INDEX_IN_REGISTER = 2;
    public static final int PASSWORD_INDEX_IN_REGISTER = 3;

    public static final int ARGUMENTS_COUNT_FOR_LOGIN_COMMAND = 2;
    public static final int USERNAME_INDEX_IN_LOGIN = 0;
    public static final int PASSWORD_INDEX_IN_LOGIN = 1;

    public static final int ARGUMENTS_COUNT_FOR_ADD_FRIEND_COMMAND = 1;
    public static final int USERNAME_INDEX_IN_ADD_FRIEND = 0;

    public static final int ARGUMENTS_COUNT_FOR_SPLIT_COMMAND = 3;
    public static final int USERNAME_INDEX_IN_SPLIT = 0;
    public static final int AMOUNT_INDEX_IN_SPLIT = 1;
    public static final int REASON_INDEX_IN_SPLIT = 2;
    public static final int ARGUMENTS_COUNT_FOR_PAYED_COMMAND = 2;

    public static final int USERNAME_INDEX_IN_PAYED = 0;
    public static final int AMOUNT_INDEX_IN_PAYED = 1;

    public static final int ARGUMENTS_COUNT_FOR_CREATE_GROUP_COMMAND = 2;

    public static final int GROUP_NAME_INDEX_IN_CREATE_GROUP = 0;
    public static final int GROUP_FIRST_MEMBER_INDEX_IN_CREATE_GROUP = 1;

    public static final int ARGUMENTS_COUNT_FOR_SPLIT_GROUP_COMMAND = 3;

    public static final int GROUP_NAME_INDEX_IN_SPLIT_GROUP = 0;
    public static final int AMOUNT_INDEX_IN_SPLIT_GROUP = 1;
    public static final int REASON_INDEX_IN_SPLIT_GROUP = 2;

    public static final int ARGUMENTS_COUNT_FOR_PAYED_GROUP_COMMAND = 3;

    public static final int GROUP_NAME_INDEX_IN_PAYED_GROUP = 0;
    public static final int USERNAME_INDEX_IN_PAYED_GROUP = 1;
    public static final int AMOUNT_INDEX_IN_PAYED_GROUP = 2;

    public static final int LOAD_DATABASE_FIRSTNAME_INDEX = 0;
    public static final int LOAD_DATABASE_LASTNAME_INDEX = 1;
    public static final int LOAD_DATABASE_USERNAME_INDEX = 2;
    public static final int LOAD_DATABASE_PASSWORD_INDEX = 3;

    public static final int LOAD_LOGS_USERNAME_INDEX = 0;

    public static final int LOAD_FRIENDS_USERNAME_INDEX = 0;
    public static final int LOAD_FRIENDS_FRIEND_USERNAME_INDEX = 1;
    public static final int LOAD_FRIENDS_PAYED_BY_OWNER_INDEX = 2;
    public static final int LOAD_FRIENDS_PAYED_BY_FRIEND_INDEX = 3;

    public static final int LOAD_GROUPS_GROUP_NAME_INDEX = 0;
    public static final int LOAD_GROUPS_OWNER_USERNAME_INDEX = 1;
    public static final int LOAD_GROUPS_PAYED_BY_OWNER_INDEX = 2;
    public static final int LOAD_GROUPS_FIRST_MEMBER_INDEX = 3;

}
