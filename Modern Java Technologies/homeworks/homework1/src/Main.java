import bg.sofia.uni.fmi.mjt.mail.Mail;
import bg.sofia.uni.fmi.mjt.mail.MailClient;
import bg.sofia.uni.fmi.mjt.mail.Outlook;

import java.util.Collection;

public class Main {
    public static void main(String[] args) {
        MailClient client = new Outlook();

        client.addNewAccount("Ivan", "ivan@mail.com");
        client.addNewAccount("Gosh", "Gosh@mail.com");
        client.addNewAccount("Sender", "stoyo@fmi.bg");
        client.createFolder("Ivan", "/inbox/important");
        client.createFolder("Ivan", "/inbox/important/veryImportant");
        client.createFolder("Ivan", "/inbox/important/veryImportant/veryVeryImportant");
        client.createFolder("Gosh", "/inbox/misc");
        client.createFolder("Gosh", "/inbox/misc/random");
        client.createFolder("Gosh", "/inbox/misc/random/random2");
        client.createFolder("Gosh", "/inbox/misc/random/photos");
        client.createFolder("Gosh", "/inbox/misc/random/photos/2012");
        client.createFolder("Gosh", "/inbox/misc/random/photos/2013");

        client.addRule("Ivan", "/inbox/important/veryImportant",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: ivan@mail.com, test@abc.bc", 1);

        client.addRule("Ivan", "/inbox/important",
            "subject-includes: mjt, izpit, 2022" + System.lineSeparator() +
                "subject-or-body-includes: izpit" + System.lineSeparator() +
                "from: stoyo@fmi.bg" + System.lineSeparator() + "recipients-includes: test1@abv.bg, ivan@mail.com", 5);

        client.receiveMail("Ivan", "sender: Gosh@mail.com" + System.lineSeparator() +
            "subject: Hello, MJT!" + System.lineSeparator() +
            "recipients: pesho@gmail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");

        client.receiveMail("Ivan", "sender: Gosh@mail.com" + System.lineSeparator() +
            "subject: Helloo, MJT!" + System.lineSeparator() +
            "recipients: pesho@gmail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Where there is desire, there is a way.");

        client.sendMail("Sender", "subject: mjt izpit 2022!" + System.lineSeparator() +
            "recipients: ivan@mail.com, gosho@gmail.com" + System.lineSeparator() +
            "received: 2022-12-08 14:14", "Hello, MJT! I am so excited for the exam!");


        Collection<Mail> ivan = client.getMailsFromFolder("Ivan", "/inbox/important/veryImportant");
        ivan.forEach(System.out::println);
    }
}