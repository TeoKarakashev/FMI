import bg.sofia.uni.fmi.mjt.sentiment.MovieReviewSentimentAnalyzer;
import bg.sofia.uni.fmi.mjt.sentiment.SentimentAnalyzer;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            SentimentAnalyzer sentimentAnalyzer = new MovieReviewSentimentAnalyzer(
                new FileReader(
                    "C:\\Users\\teodor\\Documents\\GitHub\\Java\\homework2\\src\\bg\\sofia\\uni\\fmi" +
                        "\\mjt\\sentiment\\resources\\stopwords.txt"),
                new FileReader(
                    "C:\\Users\\teodor\\Documents\\GitHub\\Java\\homework2\\src\\bg\\sofia\\uni\\fmi\\mjt" +
                        "\\sentiment\\resources\\movieReviews.txt"),
                new FileWriter(
                    "C:\\Users\\teodor\\Documents\\GitHub\\Java\\homework2\\src\\bg\\sofia\\uni\\fmi" +
                        "\\mjt\\sentiment\\resources\\movieReviews.txt",
                    true));
            System.out.println(sentimentAnalyzer.getReviewSentiment("A series of escapades demonstrating " +
                "the adage that what is good for" +
                " the goose is also good for the gander , some of which occasionally amuses but none " +
                "of which amounts to much of a story ."));
            System.out.println(sentimentAnalyzer.getWordSentiment("series") + "->" + "series");
            System.out.println(sentimentAnalyzer.getWordSentiment("escapades") + "->" + "escapades");
            System.out.println(sentimentAnalyzer.getWordSentiment("demonstrating") + "->" + "demonstrating");
            System.out.println(sentimentAnalyzer.getWordSentiment("adage") + "->" + "adage");
            System.out.println(sentimentAnalyzer.getWordSentiment("good") + "->" + "good");
            System.out.println(sentimentAnalyzer.getWordSentiment("gander") + "->" + "gander");
            System.out.println(sentimentAnalyzer.getWordSentiment("goose") + "->" + "goose");
            System.out.println(sentimentAnalyzer.getWordSentiment("also") + "->" + "also");
            System.out.println(sentimentAnalyzer.getWordSentiment("occasionally") + "->" + "occasionally");
            System.out.println(sentimentAnalyzer.getWordSentiment("amuses") + "->" + "amuses");
            System.out.println(sentimentAnalyzer.getWordSentiment("none") + "->" + "none");
            System.out.println(sentimentAnalyzer.getWordSentiment("amounts") + "->" + "amounts");
            System.out.println(sentimentAnalyzer.getWordSentiment("much") + "->" + "much");
            System.out.println(sentimentAnalyzer.getWordSentiment("story") + "->" + "story");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}