package bg.sofia.uni.fmi.mjt.mail;

import java.util.Set;

public record Rule(Account accountAppliedTo, Folder folder, Set<String> subjectKeyWords,
                   Set<String> bodyAndSubjectKeyWords, Set<String> recipientsInclude, String senderEmail,
                   int priority) {
}

