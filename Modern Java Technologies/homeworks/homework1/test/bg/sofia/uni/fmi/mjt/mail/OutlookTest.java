package bg.sofia.uni.fmi.mjt.mail;


import bg.sofia.uni.fmi.mjt.mail.exceptions.AccountAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.AccountNotFoundException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.FolderAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.FolderNotFoundException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.InvalidPathException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.RuleAlreadyDefinedException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class OutlookTest {

    private MailClient outlook;

    @BeforeEach
    public void setUp() {
        outlook = new Outlook();
    }

    @AfterEach
    public void tearDown() {
        outlook = null;
    }

    @Test
    public void testAddNewAccountShouldThrowWithAccountNameNull() {
        assertThrows(IllegalArgumentException.class, () -> outlook.addNewAccount(null, "test@.bg"),
            "Cannot add account with null name");
    }

    @Test
    public void testAddNewAccountShouldThrowWithAccountNameBlank() {
        assertThrows(IllegalArgumentException.class, () -> outlook.addNewAccount("", "test@.bg"),
            "Cannot add account with blank name");
    }

    @Test
    public void testAddNewAccountShouldThrowWithEmailNameBlank() {
        assertThrows(IllegalArgumentException.class, () -> outlook.addNewAccount("test", "  "),
            "Cannot add account with blank email");
    }

    @Test
    public void testAddNewAccountShouldThrowWithEmailNameNull() {
        assertThrows(IllegalArgumentException.class, () -> outlook.addNewAccount("test", null),
            "Cannot add account with null email");
    }

    @Test
    public void testAddNewAccountShouldThrowWithAccountWithSameName() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(AccountAlreadyExistsException.class, () -> outlook.addNewAccount("test", "test2@mail.com"));
    }

    @Test
    public void testAddNewAccountShouldThrowWithAccountWithSameEmail() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.addNewAccount("test2", "test2@mail.com");
        assertThrows(AccountAlreadyExistsException.class, () -> outlook.addNewAccount("test3", "test2@mail.com"));
    }


    @Test
    public void testCreateFolderShouldThrowWithAnyParamNull() {
        assertThrows(IllegalArgumentException.class, () -> outlook.createFolder(null, "/inboc/test"),
            "Cannot create folder with null acc name");

        assertThrows(IllegalArgumentException.class, () -> outlook.createFolder("pesho", null),
            "Cannot create folder with null path");
    }

    @Test
    public void testCreateFolderShouldThrowWithNonExistentAccount() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(AccountNotFoundException.class, () -> outlook.createFolder("pesho", "/inbox/test"),
            "Cannot create folder with non existent account");
    }

    @Test
    public void testCreateFolderShouldThrowWithPathNotStartingFromTheRootFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(InvalidPathException.class, () -> outlook.createFolder("test", "inbox/test"),
            "Path should start with /inbox");
        assertThrows(InvalidPathException.class, () -> outlook.createFolder("test", "/sent/test"),
            "Path should start with /inbox");
        assertThrows(InvalidPathException.class, () -> outlook.createFolder("test", "/test"),
            "Path should start with /inbox");
    }

    @Test
    void testCreateFolderShouldThrowWithIntermediateFolderMissing() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(InvalidPathException.class, () -> outlook.createFolder("test", "/inbox/test/test2"),
            "Path should contain all intermediate folders");
        outlook.createFolder("test", "/inbox/test");
        assertThrows(InvalidPathException.class, () -> outlook.createFolder("test", "/inbox/test/test2/test3"),
            "Path should contain all intermediate folders");
    }

    @Test
    void testCreateFolderShouldThrowWithAlreadyExistingFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.createFolder("test", "/inbox/test");
        assertThrows(FolderAlreadyExistsException.class, () -> outlook.createFolder("test", "/inbox/test"),
            "cannot create folder with already existing name");
        outlook.createFolder("test", "/inbox/test/test2");
        assertThrows(FolderAlreadyExistsException.class, () -> outlook.createFolder("test", "/inbox/test/test2"),
            "cannot create folder with already existing name");
    }

    @Test
    public void testAddRuleShouldThrowWithAnyNullParams() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(IllegalArgumentException.class, () -> outlook.addRule(null, "/inbox/important/veryImportant",
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", 5),
            "Cannot with null account name");

        assertThrows(IllegalArgumentException.class, () -> outlook.addRule("test", null,
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", 5),
            "Cannot with null path");

        assertThrows(IllegalArgumentException.class, () -> outlook.addRule("test", "/inbox/important/veryImportant",
                null, 5),
            "Cannot with null rule definition");

    }

    @Test
    public void testAddRuleShouldThrowWithInvalidPriority() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(IllegalArgumentException.class, () -> outlook.addRule("test", "/inbox/important/veryImportant",
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", -1),
            "Priority cannot be negative");

        assertThrows(IllegalArgumentException.class, () -> outlook.addRule("test", "/inbox/important/veryImportant",
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", 11),
            "Priority cannot be greater than 10");
    }

    @Test
    public void testAddRuleShouldThrowWithAccountNotPresent() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(AccountNotFoundException.class, () -> outlook.addRule("ivan", "/inbox/important/veryImportant",
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", 5),
            "Cannot add rule to non existent account");
    }

    @Test
    public void testAddRuleShouldThrowWithInvalidPath() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(FolderNotFoundException.class, () -> outlook.addRule("test", "/inbox/important",
                "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                    "subject-or-body-includes: izpit" + System.lineSeparator() +
                    "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, test@abc.bc", 5),
            "Cannot add rule to non existent folder");
    }

    @Test
    public void testAddRuleShouldSendEmailFromRootFolderToTheNewFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.addNewAccount("sender", "sender@mail.com");
        outlook.createFolder("test", "/inbox/important");

        outlook.receiveMail("test", "sender: sender@mail.com" + System.lineSeparator() +
            "subject: Hello this is mjt izpit 2022" + System.lineSeparator() +
            "recipients: ivan@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");
        assertEquals(1, outlook.getMailsFromFolder("test", "/inbox").size(), "Should have 1 mail in inbox");

        outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bv",
            3);

        assertEquals(0, outlook.getMailsFromFolder("test", "/inbox").size(), "Mail should be moved to the new folder");
        assertEquals(1, outlook.getMailsFromFolder("test", "/inbox/important").size(),
            "Mail should be moved to the new folder");
    }

    @Test
    public void testAddRuleShouldThrowWithMailAnyMailMetadataDefinedMoreThanOnce() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.createFolder("test", "/inbox/important");

        assertThrows(RuleAlreadyDefinedException.class, () -> outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bv" +
                System.lineSeparator() + "subject-includes: mjt, izpit, 2022",
            3), "Cannot add rule with mail metadata defined more than once");

        assertThrows(RuleAlreadyDefinedException.class, () -> outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bv" +
                System.lineSeparator() + "subject-or-body-includes: izpit",
            3), "Cannot add rule with mail metadata defined more than once");

        assertThrows(RuleAlreadyDefinedException.class, () -> outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bv" +
                System.lineSeparator() + "from: tester@abv.bg",
            3), "Cannot add rule with mail metadata defined more than once");
        assertThrows(RuleAlreadyDefinedException.class, () -> outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bv" +
                System.lineSeparator() + "recipients-includes: tester@abv.bg",
            3), "Cannot add rule with mail metadata defined more than once");
    }

    @Test
    public void testGetMailsFromFolderShouldThrowWithInvalidParameters() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(IllegalArgumentException.class, () -> outlook.getMailsFromFolder("", "/inbox/important"),
            "Account name cannot be empty");
        assertThrows(IllegalArgumentException.class, () -> outlook.getMailsFromFolder("test", null),
            "Folder path cannot be null");
        assertThrows(AccountNotFoundException.class, () -> outlook.getMailsFromFolder("test2", "/inbox/important"),
            "cannot get mails from non existent account");
    }

    @Test
    public void testGetMailsFromFolderShouldThrowWithNonExistentAccount() {
        assertThrows(AccountNotFoundException.class, () -> outlook.getMailsFromFolder("test", "/inbox/important"),
            "cannot get mails from non existent account");
    }

    @Test
    public void testGetMailsFromFolderShouldThrowWithNonExistentFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        assertThrows(FolderNotFoundException.class, () -> outlook.getMailsFromFolder("test", "/inbox/important"),
            "cannot get mails from non existent account");
    }

    @Test
    public void testReceiveMailShouldThrowWithInvalidParameters() {
        assertThrows(IllegalArgumentException.class, () -> outlook.receiveMail("", "asdas", "asd"),
            "Account name cannot be empty or null");
        assertThrows(IllegalArgumentException.class, () -> outlook.receiveMail("name", null, "asd"),
            "mail metadata cannot be empty or null");
        assertThrows(IllegalArgumentException.class, () -> outlook.receiveMail("name", "asdas", ""),
            "mail content cannot be empty or null");
    }

    @Test
    public void testGetMailsFromFolderShouldReturnAllMailsFromFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.addNewAccount("sender", "sender@mail.com");
        outlook.createFolder("test", "/inbox/important");

        outlook.receiveMail("test", "sender: sender@mail.com" + System.lineSeparator() +
            "subject: Hello this is mjt izpit 2022" + System.lineSeparator() +
            "recipients: ivan@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");
        assertEquals(1, outlook.getMailsFromFolder("test", "/inbox").size(), "Should have 1 mail in inbox");
    }

    @Test
    public void testSendMailShouldThrowWithInvalidParameters() {
        assertThrows(IllegalArgumentException.class, () -> outlook.sendMail(null, "asd", "sdad"),
            "Account name cannot be empty or null");
        assertThrows(IllegalArgumentException.class, () -> outlook.sendMail("test", "", "sadsa"),
            "Mail metadata cannot be empty or null");
        assertThrows(IllegalArgumentException.class, () -> outlook.sendMail("test", "asdsada", null),
            "Mail body cannot be empty or null");
    }

    @Test
    public void testSendMailShouldAddTheMailInSenderFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.addNewAccount("sender", "sender@mail.com");
        outlook.sendMail("sender", "subject: mjt izpit 2022!" + System.lineSeparator() +
            "recipients: ivan@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");
        assertEquals(1, outlook.getMailsFromFolder("sender", "/sent").size(), "Should have 1 mail in inbox");
    }

    @Test
    public void testSendMailShouldAddTheMailInRecipientsFolder() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.addNewAccount("sender", "sender@mail.com");
        outlook.sendMail("sender", "subject: mjt izpit 2022!" + System.lineSeparator() +
            "recipients: test@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");
        assertEquals(1, outlook.getMailsFromFolder("test", "/inbox").size(), "Should have 1 mail in inbox");
    }

    @Test
    public void testSendMailShouldBeSentToCorrectFolderFromRule() {
        outlook.addNewAccount("test", "test@mail.com");
        outlook.createFolder("test", "/inbox/important");
        outlook.addNewAccount("sender", "sender@mail.com");
        outlook.addRule("test", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: sender@mail.com" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@mail.com",
            3);
        outlook.sendMail("sender", "subject: mjt izpit 2022!" + System.lineSeparator() +
            "recipients: test@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");
        assertEquals(0, outlook.getMailsFromFolder("test", "/inbox").size(),
            "Recieved mail should be sent to correct folder");
        assertEquals(1, outlook.getMailsFromFolder("test", "/inbox/important").size(),
            "Recieved mail should be sent to correct folder");
    }
}