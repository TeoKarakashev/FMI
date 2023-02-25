package bg.sofia.uni.fmi.mjt.sentiment;

import bg.sofia.uni.fmi.mjt.sentiment.container.WordDataContainer;
import bg.sofia.uni.fmi.mjt.sentiment.reader.StopWordsReader;
import bg.sofia.uni.fmi.mjt.sentiment.analyzer.WordsAnalyzer;
import bg.sofia.uni.fmi.mjt.sentiment.analyzer.WordsAnalyzerAPI;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class MovieReviewSentimentAnalyzer implements SentimentAnalyzer {

    private static final int SENTIMENT_MAX_VALUE = 4;

    private final List<String> stopWords;
    private Map<String, WordDataContainer> wordsSentiment;
    private final WordsAnalyzerAPI wordsAnalyzer;
    private Writer writer;

    public MovieReviewSentimentAnalyzer(Reader stopwordsIn, Reader reviewsIn, Writer reviewsOut) {
        wordsAnalyzer = new WordsAnalyzer();
        stopWords = new StopWordsReader().readStopWords(stopwordsIn);

        try (var bufferedReader = new BufferedReader(reviewsIn)) {
            var reviews = bufferedReader.lines().collect(Collectors.toList());
            wordsSentiment = wordsAnalyzer.getWordsSentiment(reviews, stopWords);

        } catch (IOException e) {
            throw new IllegalStateException("A problem occurred while reading from a file", e);
        }

        writer = new BufferedWriter(reviewsOut);

    }

    @Override
    public double getReviewSentiment(String review) {
        return wordsAnalyzer.reviewSentiment(review, wordsSentiment, stopWords);
    }

    @Override
    public String getReviewSentimentAsName(String review) {
        return wordsAnalyzer.reviewSentimentAsName(review, wordsSentiment, stopWords);
    }

    @Override
    public double getWordSentiment(String word) {
        if (wordsSentiment.containsKey(word.toLowerCase())) {
            return wordsSentiment.get(word.toLowerCase()).getSentiment();
        }
        return -1.0;
    }

    @Override
    public int getWordFrequency(String word) {
        if (wordsSentiment.containsKey(word.toLowerCase())) {
            return wordsSentiment.get(word.toLowerCase()).getFrequency();
        }
        return 0;
    }

    @Override
    public List<String> getMostFrequentWords(int n) {

        if (n < 0) {
            throw new IllegalArgumentException("n must be positive");
        }

        return wordsSentiment.entrySet().stream()
            .sorted((e1, e2) -> e2.getValue().getFrequency() - e1.getValue().getFrequency())
            .limit(n)
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
    }

    @Override
    public List<String> getMostPositiveWords(int n) {

        if (n < 0) {
            throw new IllegalArgumentException("n must be positive");
        }

        return wordsSentiment.entrySet().stream()
            .sorted((e1, e2) -> Double.compare(e2.getValue().getSentiment(), e1.getValue().getSentiment()))
            .limit(n)
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
    }

    @Override
    public List<String> getMostNegativeWords(int n) {

        if (n < 0) {
            throw new IllegalArgumentException("n must be positive");
        }

        return wordsSentiment.entrySet().stream()
            .sorted(Comparator.comparingDouble(e2 -> e2.getValue().getSentiment()))
            .limit(n)
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
    }

    @Override
    public boolean appendReview(String review, int sentiment) {
        if (review == null || review.isBlank()) {
            throw new IllegalArgumentException("Review cannot be null, empty or black");
        }

        if (sentiment < 0 || sentiment > SENTIMENT_MAX_VALUE) {
            throw new IllegalArgumentException("Sentiment must be between 0 and 10");
        }

        String reviewWithSentiment = sentiment + " " + review;
        try {
            writer.write(reviewWithSentiment);
            writer.write(System.lineSeparator());
            writer.flush();
        } catch (IOException e) {
            return false;
        }
        wordsAnalyzer.addReview(reviewWithSentiment, wordsSentiment, stopWords);

        return true;
    }

    @Override
    public int getSentimentDictionarySize() {
        return wordsSentiment.size();
    }

    @Override
    public boolean isStopWord(String word) {
        return stopWords.contains(word);
    }
}
