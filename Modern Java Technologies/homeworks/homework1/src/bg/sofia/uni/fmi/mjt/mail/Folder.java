package bg.sofia.uni.fmi.mjt.mail;

import java.util.HashSet;
import java.util.Set;

public record Folder(String name, Set<Folder> subFolders, Set<Mail> mails) {
    public Folder(String name) {
        this(name, new HashSet<>(), new HashSet<>());
    }

    public void addFolder(Folder folder) {
        subFolders.add(folder);
    }

    public void addMail(Mail mail) {
        mails.add(mail);
    }

}

