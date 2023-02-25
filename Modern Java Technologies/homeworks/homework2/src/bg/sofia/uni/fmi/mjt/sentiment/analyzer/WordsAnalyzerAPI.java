package bg.sofia.uni.fmi.mjt.sentiment.analyzer;

import bg.sofia.uni.fmi.mjt.sentiment.container.WordDataContainer;

import java.util.List;
import java.util.Map;

public interface WordsAnalyzerAPI {

    Map<String, WordDataContainer> getWordsSentiment(List<String> reviews, List<String> stopWords);

    double reviewSentiment(String review, Map<String, WordDataContainer> wordsSentiment, List<String> stopWords);

    String reviewSentimentAsName(String review, Map<String, WordDataContainer> wordsSentiment, List<String> stopWords);

    void addReview(String review, Map<String, WordDataContainer> wordsSentiment, List<String> stopWords);

}
