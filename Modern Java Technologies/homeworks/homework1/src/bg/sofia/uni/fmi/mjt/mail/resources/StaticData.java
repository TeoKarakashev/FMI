package bg.sofia.uni.fmi.mjt.mail.resources;

public class StaticData {
    public static final String PATH_SEPARATOR = "/";
    public static final String RULE_SEPARATOR = ": ";
    public static final String SPLIT_DIFFERENT_WORDS_IN_RULE = ", ";

    public static final String ROOT_FOLDER_PATH = "/inbox";
    public static final String SENT_FOLDER_PATH = "/sent";
    public static final int MIN_SIZE_OF_FOLDER_SIZE_WITH_EMPTY_ELEMENTS_IN_FRONT = 3;
    public static final int INDEX_OF_ROOT_FOLDER_WITH_EMPTY_FIRST = 1;
    public static final int INDEX_OF_EMPTY_FOLDER_PATH = 0;

    public static final int INDEX_OF_COMMAND = 0;

    public static final int INDEX_OF_DATA = 1;

    public static final int PRIORITY_MIN_VALUE = 1;
    public static final int PRIORITY_MAX_VALUE = 10;

    public static final String DATE_FORMAT = "yyyy-MM-dd HH:mm";
}
