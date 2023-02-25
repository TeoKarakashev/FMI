package bg.sofia.uni.fmi.mjt.sentiment.reader;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ReviewReader implements ReviewReaderAPI {

    private static final int REVIEW_SCORE_POSITION = 0;
    private static final String WORDS_REGEX = "[^a-zA-Z0-9']";

    @Override
    public Set<String> parseReview(String review, List<String> stopWords) {
        return Arrays.stream(review.split(WORDS_REGEX))
                .map(String::toLowerCase)
                .filter(word -> !stopWords.contains(word) && word.length() > 1)
                .collect(Collectors.toSet());
    }

    @Override
    public List<String> parseReviewToBeGraded(String review, List<String> stopWords) {
        return Arrays.stream(review.split(WORDS_REGEX))
            .map(String::toLowerCase)
            .filter(word -> !stopWords.contains(word) && word.length() > 1)
            .collect(Collectors.toList());
    }

    @Override
    public double getReviewScore(String review) {
        return Double.parseDouble(review.charAt(REVIEW_SCORE_POSITION) + "");
    }

}

