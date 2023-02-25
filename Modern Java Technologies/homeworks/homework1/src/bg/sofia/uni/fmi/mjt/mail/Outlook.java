package bg.sofia.uni.fmi.mjt.mail;


import bg.sofia.uni.fmi.mjt.mail.exceptions.AccountAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.AccountNotFoundException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.FolderAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.FolderNotFoundException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.InvalidPathException;
import bg.sofia.uni.fmi.mjt.mail.exceptions.RuleAlreadyDefinedException;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.DATE_FORMAT;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.INDEX_OF_COMMAND;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.INDEX_OF_DATA;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.INDEX_OF_EMPTY_FOLDER_PATH;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.INDEX_OF_ROOT_FOLDER_WITH_EMPTY_FIRST;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.MIN_SIZE_OF_FOLDER_SIZE_WITH_EMPTY_ELEMENTS_IN_FRONT;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.PATH_SEPARATOR;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.PRIORITY_MAX_VALUE;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.PRIORITY_MIN_VALUE;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.ROOT_FOLDER_PATH;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.RULE_SEPARATOR;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.SENT_FOLDER_PATH;
import static bg.sofia.uni.fmi.mjt.mail.resources.StaticData.SPLIT_DIFFERENT_WORDS_IN_RULE;


public class Outlook implements MailClient {

    private Set<Account> accounts;
    private Map<String, Set<Folder>> folders;
    private Map<String, Set<Rule>> rules;

    public Outlook() {
        accounts = new HashSet<>();
        folders = new HashMap<>();
        rules = new HashMap<>();
    }

    private boolean isParamValid(String param) {
        return param != null && !param.isBlank();
    }

    private boolean isAccountPresent(String name, String email) {
        for (Account account : accounts) {
            if (account.name().equals(name) || account.emailAddress().equals(email)) {
                return true;
            }
        }
        return false;
    }

    private boolean isAccountPresent(String name) {
        for (Account account : accounts) {
            if (account.name().equals(name)) {
                return true;
            }
        }
        return false;
    }

    private boolean isAccountPresentByMail(String mail) {
        for (Account account : accounts) {
            if (account.emailAddress().equals(mail)) {
                return true;
            }
        }
        return false;
    }

    private Account getAccountByName(String name) {
        for (Account account : accounts) {
            if (account.name().equals(name)) {
                return account;
            }
        }
        return null;
    }

    private Account getAccountByEmail(String email) {
        for (Account account : accounts) {
            if (account.emailAddress().equals(email)) {
                return account;
            }
        }
        return null;
    }

    private boolean createNewAccountFolders(String name) {
        if (!isAccountPresent(name)) {
            return false;
        }
        folders.put(name, new HashSet<>());
        folders.get(name).add(new Folder("inbox"));
        folders.get(name).add(new Folder("sent"));
        return true;
    }

    @Override
    public Account addNewAccount(String accountName, String email) {
        if (!isParamValid(accountName) || !isParamValid(email)) {
            throw new IllegalArgumentException("Invalid account name or email");
        }
        if (isAccountPresent(accountName, email)) {
            throw new AccountAlreadyExistsException("Account with same name or email already exists");
        }

        Account newAccount = new Account(email, accountName);
        accounts.add(newAccount);
        createNewAccountFolders(accountName);
        rules.put(accountName, new HashSet<>());

        return newAccount;
    }

    private Folder getLatestParent(Set<Folder> rootFolder, List<String> path) {

        List<String> subPath = new ArrayList<>(path);
        if (path.size() >= MIN_SIZE_OF_FOLDER_SIZE_WITH_EMPTY_ELEMENTS_IN_FRONT) {
            subPath = subPath.subList(0, path.size() - 1);
        }

        subPath.remove(INDEX_OF_EMPTY_FOLDER_PATH);
        Folder latestParent = null;
        for (String s : subPath) {
            boolean isFound = false;
            for (Folder folder : rootFolder) {
                if (folder.name().equals(s)) {
                    isFound = true;
                    latestParent = folder;
                    rootFolder = folder.subFolders();
                    break;
                }
            }
            if (!isFound) {
                throw new InvalidPathException("Path does not exist");
            }
        }

        return latestParent;
    }


    @Override
    public void createFolder(String accountName, String path) {
        if (!isParamValid(accountName) || !isParamValid(path)) {
            throw new IllegalArgumentException("Invalid account name or path");
        }
        if (!isAccountPresent(accountName)) {
            throw new AccountNotFoundException("Account does not exist");
        }

        List<String> pathList = new ArrayList<>(Arrays.asList(path.split(PATH_SEPARATOR)));

        if (pathList.size() < MIN_SIZE_OF_FOLDER_SIZE_WITH_EMPTY_ELEMENTS_IN_FRONT) {
            throw new InvalidPathException("Path is invalid");
        }

        if (!pathList.get(INDEX_OF_ROOT_FOLDER_WITH_EMPTY_FIRST).equals("inbox")) {
            throw new InvalidPathException("Path does not start from the root folder");
        }

        Folder latestParentFolder = getLatestParent(this.folders.get(accountName), pathList);

        for (Folder folder : latestParentFolder.subFolders()) {
            if (folder.name().equals(pathList.get(pathList.size() - 1))) {
                throw new FolderAlreadyExistsException("Path already exists");
            }
        }

        latestParentFolder.addFolder(new Folder(pathList.get(pathList.size() - 1)));
    }

    private Folder getFolder(String accountName, String path) {

        if (path.equals(ROOT_FOLDER_PATH)) {
            for (Folder folder : folders.get(accountName)) {
                if (folder.name().equals("inbox")) {
                    return folder;
                }
            }
        }

        if (path.equals(SENT_FOLDER_PATH)) {
            for (Folder folder : folders.get(accountName)) {
                if (folder.name().equals("sent")) {
                    return folder;
                }
            }
        }

        List<String> pathList = new ArrayList<>(Arrays.asList(path.split(PATH_SEPARATOR)));
        Folder latestParentFolder = getLatestParent(this.folders.get(accountName), pathList);

        for (Folder folder : latestParentFolder.subFolders()) {
            if (folder.name().equals(pathList.get(pathList.size() - 1))) {
                return folder;
            }
        }
        return null;
    }

    private Set<String> getMetaData(String line) {
        if (line != null) {
            return new HashSet<>(Arrays.asList(line.split(SPLIT_DIFFERENT_WORDS_IN_RULE)));
        }
        return null;
    }

    private boolean canApplyRule(Mail mail, Rule rule) {
        if (!mail.sender().emailAddress().equals(rule.senderEmail())) {
            return false;
        }

        for (String subjectKeyWord : rule.subjectKeyWords()) {
            if (!mail.subject().contains(subjectKeyWord)) {
                return false;
            }
        }

        int count = 0;
        for (String recipient : mail.recipients()) {
            for (String s : rule.recipientsInclude()) {
                if (recipient.equals(s)) {
                    count++;
                }
            }
        }
        if (count == 0) {
            return false;
        }

        for (String bodyAndSubjectKeyWord : rule.bodyAndSubjectKeyWords()) {
            if (!mail.subject().contains(bodyAndSubjectKeyWord) && !mail.body().contains(bodyAndSubjectKeyWord)) {
                return false;
            }
        }
        return true;
    }


    private boolean applyRule(Mail mail, Rule rule) {

        if (!canApplyRule(mail, rule)) {
            return false;
        }

        rule.folder().addMail(mail);

        return true;
    }


    @Override
    public void addRule(String accountName, String folderPath, String ruleDefinition, int priority) {
        if (!isParamValid(accountName) || !isParamValid(folderPath) ||
            !isParamValid(ruleDefinition) || priority < PRIORITY_MIN_VALUE || priority > PRIORITY_MAX_VALUE) {
            throw new IllegalArgumentException("Invalid account name, path, rule definition, priority");
        }
        if (!isAccountPresent(accountName)) {
            throw new AccountNotFoundException("Account does not exist");
        }
        Folder folder = getFolder(accountName, folderPath);

        if (folder == null) {
            throw new FolderNotFoundException("Folder does not exist");
        }

        List<String> ruleDefinitionList = new ArrayList<>(Arrays.asList(ruleDefinition.split(System.lineSeparator())));
        Set<String> subjectKeyWordsSet = null;
        Set<String> subjectOrBodyKeyWordsSet = null;
        Set<String> recipientsSet = null;
        String sender = null;
        for (String s : ruleDefinitionList) {
            List<String> currentLine = new ArrayList<>(Arrays.asList(s.split(RULE_SEPARATOR)));
            switch (currentLine.get(INDEX_OF_COMMAND)) {
                case "subject-includes" -> {
                    if (subjectKeyWordsSet != null) {
                        throw new RuleAlreadyDefinedException("subject-includes is defined more than once");
                    }
                    subjectKeyWordsSet = getMetaData(currentLine.get(INDEX_OF_DATA));
                }
                case "subject-or-body-includes" -> {
                    if (subjectOrBodyKeyWordsSet != null) {
                        throw new RuleAlreadyDefinedException("subject-or-body-includes is defined more than once");
                    }
                    subjectOrBodyKeyWordsSet = getMetaData(currentLine.get(INDEX_OF_DATA));
                }
                case "recipients-includes" -> {
                    if (recipientsSet != null) {
                        throw new RuleAlreadyDefinedException("recipients-includes is defined more than once");
                    }
                    recipientsSet = getMetaData(currentLine.get(INDEX_OF_DATA));
                }
                case "from" -> {
                    if (sender != null) {
                        throw new RuleAlreadyDefinedException("from is defined more than once");
                    }
                    sender = currentLine.get(INDEX_OF_DATA);
                }
            }
        }

        Account account = getAccountByName(accountName);

        if (subjectKeyWordsSet == null) {
            subjectKeyWordsSet = new HashSet<>();
        }
        if (subjectOrBodyKeyWordsSet == null) {
            subjectOrBodyKeyWordsSet = new HashSet<>();
        }
        if (recipientsSet == null) {
            recipientsSet = new HashSet<>();
        }
        if (sender == null) {
            sender = "";
        }

        Rule rule = new Rule(account, folder, subjectKeyWordsSet, subjectOrBodyKeyWordsSet, recipientsSet, sender,
            priority);
        rules.get(accountName).add(rule);

        Folder inbox = getFolder(accountName, ROOT_FOLDER_PATH);

        Set<Mail> toRemove = new HashSet<>();
        for (Mail mail : inbox.mails()) {
            if (applyRule(mail, rule)) {
                toRemove.add(mail);
            }
        }
        inbox.mails().removeAll(toRemove);
    }

    private Mail parseMailMetadata(String mailMetadata, String mailContent) {
        List<String> mailMetadataList = new ArrayList<>(Arrays.asList(mailMetadata.split(System.lineSeparator())));
        String sender = null;
        Set<String> recipients = null;
        String time = null;
        String subject = null;

        for (String s : mailMetadataList) {
            List<String> currentLine = new ArrayList<>(Arrays.asList(s.split(RULE_SEPARATOR)));
            switch (currentLine.get(INDEX_OF_COMMAND)) {
                case "sender" -> sender = currentLine.get(INDEX_OF_DATA);
                case "recipients" -> recipients = getMetaData(currentLine.get(INDEX_OF_DATA));
                case "received" -> time = currentLine.get(INDEX_OF_DATA);
                case "subject" -> subject = currentLine.get(INDEX_OF_DATA);
            }
        }
        LocalDateTime dateTime = LocalDateTime.parse(time, DateTimeFormatter.ofPattern(DATE_FORMAT));
        Account senderAcc = getAccountByEmail(sender);
        if (senderAcc == null) {
            sender = "";
        }
        if (recipients == null) {
            recipients = new HashSet<>();
        }
        if (subject == null) {
            subject = "";
        }


        Mail mail = new Mail(senderAcc, recipients, subject, mailContent, dateTime);
        return mail;
    }


    @Override
    public void receiveMail(String accountName, String mailMetadata, String mailContent) {
        if (!isParamValid(accountName) || !isParamValid(mailMetadata) || !isParamValid(mailContent)) {
            throw new IllegalArgumentException("Invalid account name, mail metadata, mail content");
        }
        if (!isAccountPresent(accountName)) {
            throw new AccountNotFoundException("Account does not exist");
        }

        Mail mail = parseMailMetadata(mailMetadata, mailContent);

        Folder inbox = getFolder(accountName, ROOT_FOLDER_PATH);

        List<Rule> rulesList = new ArrayList<>(rules.get(accountName));
        rulesList.sort(Comparator.comparingInt(Rule::priority));
        for (Rule rule : rulesList) {
            if (applyRule(mail, rule)) {
                return;
            }
        }
        inbox.addMail(mail);
    }

    @Override
    public Collection<Mail> getMailsFromFolder(String account, String folderPath) {
        if (!isParamValid(account) || !isParamValid(folderPath)) {
            throw new IllegalArgumentException("Invalid account name or folder path");
        }
        if (!isAccountPresent(account)) {
            throw new AccountNotFoundException("Account does not exist");
        }
        Folder folder = getFolder(account, folderPath);
        if (folder == null) {
            throw new FolderNotFoundException("Folder does not exist");
        }

        return folder.mails();
    }

    @Override
    public void sendMail(String accountName, String mailMetadata, String mailContent) {
        if (!isParamValid(accountName) || !isParamValid(mailMetadata) || !isParamValid(mailContent)) {
            throw new IllegalArgumentException("Invalid account name, mail metadata, mail content");
        }

        if (!mailMetadata.contains("sender")) {
            Account account = getAccountByName(accountName);
            mailMetadata = mailMetadata + System.lineSeparator() + "sender: " + account.emailAddress();
        }

        Mail mail = parseMailMetadata(mailMetadata, mailContent);

        Folder sent = getFolder(accountName, SENT_FOLDER_PATH);
        sent.addMail(mail);

        for (String recipient : mail.recipients()) {
            if (isAccountPresentByMail(recipient)) {
                Account account = getAccountByEmail(recipient);
                receiveMail(account.name(), mailMetadata, mailContent);
            }
        }

    }
}
