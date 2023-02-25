package bg.sofia.uni.fmi.mjt.sentiment.analyzer;

import bg.sofia.uni.fmi.mjt.sentiment.container.WordDataContainer;
import bg.sofia.uni.fmi.mjt.sentiment.reader.ReviewReader;
import bg.sofia.uni.fmi.mjt.sentiment.reader.ReviewReaderAPI;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class WordsAnalyzer implements WordsAnalyzerAPI {
    private static final double NEGATIVE_MAX_VALUE = 0.5;
    private static final double SOMEWHAT_NEGATIVE_MAX_VALUE = 1.5;
    private static final double NEUTRAL_MAX_VALUE = 2.5;
    private static final double SOMEWHAT_POSITIVE_MAX_VALUE = 3.5;

    private final ReviewReaderAPI reviewReader;

    public WordsAnalyzer() {
        reviewReader = new ReviewReader();
    }

    @Override
    public Map<String, WordDataContainer> getWordsSentiment(List<String> reviews, List<String> stopWords) {
        Map<String, WordDataContainer> wordsSentiment = new HashMap<>();
        for (String review : reviews) {
            addReview(review, wordsSentiment, stopWords);
        }
        return wordsSentiment;
    }


    @Override
    public double reviewSentiment(String review, Map<String, WordDataContainer> wordsSentiment,
                                  List<String> stopWords) {
        List<String> words = reviewReader.parseReviewToBeGraded(review, stopWords);

        return words.stream()
            .filter(wordsSentiment::containsKey)
            .mapToDouble(word -> wordsSentiment.get(word).getSentiment())
            .average()
            .orElse(-1.0);

    }

    @Override
    public String reviewSentimentAsName(String review, Map<String, WordDataContainer> wordsSentiment,
                                        List<String> stopWords) {
        double reviewSentiment = reviewSentiment(review, wordsSentiment, stopWords);
        if (reviewSentiment == -1.0) {
            return "unknown";
        }
        if (reviewSentiment < NEGATIVE_MAX_VALUE) {
            return "negative";
        }
        if (reviewSentiment < SOMEWHAT_NEGATIVE_MAX_VALUE) {
            return "somewhat negative";
        }
        if (reviewSentiment < NEUTRAL_MAX_VALUE) {
            return "neutral";
        }
        if (reviewSentiment < SOMEWHAT_POSITIVE_MAX_VALUE) {
            return "somewhat positive";
        }
        return "positive";
    }


    @Override
    public void addReview(String review, Map<String, WordDataContainer> wordsSentiment, List<String> stopWords) {
        if (review.isBlank()) {
            return;
        }
        double reviewScore = reviewReader.getReviewScore(review);
        Set<String> words = reviewReader.parseReview(review, stopWords);
        words.forEach(word -> {
            if (wordsSentiment.containsKey(word)) {
                WordDataContainer wordDataContainer = wordsSentiment.get(word);
                wordDataContainer.addSentiment(reviewScore);
            } else {
                wordsSentiment.put(word, new WordDataContainer(reviewScore));
            }
        });
    }

}
