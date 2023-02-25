package bg.sofia.uni.fmi.mjt.sentiment;


import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MovieReviewSentimentAnalyzerTest {

    @Test
    public void testGetReviewSentimentShouldCalculateTheSentimentOfTheReview() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(2.000, analyzer.getReviewSentiment("This Movie is not bad and not a masterclass . "),
            "Sentiment is not correct");
        assertEquals(3.00, analyzer.getReviewSentiment("Movie is a masterclass ."),
            "Sentiment is not correct");
    }

    @Test
    public void testGetReviewSentimentShouldReturnNegativeValueWithNoKnownWords() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(-1.0, analyzer.getReviewSentiment("THIS iS not a TeSt and  it ROCKS . "),
            "Sentiment should return negative value with unknown words");
    }

    @Test
    public void testGetReviewSentimentAsNameShouldWorkCorrectly() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals("unknown", analyzer.getReviewSentimentAsName("THIS iS not a TeSt and  it ROCKS . "),
            "Sentiment should return unknownType value no known words");
        assertEquals("negative", analyzer.getReviewSentimentAsName("This is very very very bad not okay . "),
            "Sentiment is not parsedCorrectly two name");
        assertEquals("somewhat negative", analyzer.getReviewSentimentAsName("This is very okay . "),
            "Sentiment is not parsedCorrectly two name");
        assertEquals("neutral", analyzer.getReviewSentimentAsName("This Movie is not bad and not a masterclass . "),
            "Sentiment is not parsedCorrectly two name");
        assertEquals("somewhat positive", analyzer.getReviewSentimentAsName("Movie is masterclass and a masterpiece ."),
            "Sentiment is not parsedCorrectly two name");
        assertEquals("positive", analyzer.getReviewSentimentAsName("This is a masterclass and a masterpiece ."),
            "Sentiment is not parsedCorrectly two name");
    }

    @Test
    public void testGetWordSentimentShouldReturnNegativeWithUnknownWord() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(-1.0, analyzer.getWordSentiment("Cheese"),
            "Word sentiment should return negative value with unknown word");
    }

    @Test
    public void testGetWordSentimentShouldReturnCorrectValue() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(2.000, analyzer.getWordSentiment("Film"),
            "Word sentiment should return correct value ");
        assertEquals(4.000, analyzer.getWordSentiment("masterclass"),
            "Word sentiment should return correct value ");
        assertEquals(7/3d, analyzer.getWordSentiment("movie"),
            "Word sentiment should return correct value ");
    }

    @Test
    public void testGetWordFrequencyShouldReturnCorrectValue() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(2, analyzer.getWordFrequency("FiLm"),
            "Word sentiment should return correct value ");
        assertEquals(0, analyzer.getWordFrequency("not"),
            "Word sentiment should return correct value with word in stopWords");
        assertEquals(0, analyzer.getWordFrequency("dong"),
            "Word sentiment should return correct value with word not in database");
        assertEquals(3, analyzer.getWordFrequency("movie"),
            "Word sentiment should return correct value ");
    }

    @Test
    public void testGetMostFrequentWordsShouldThrowWithNegativeValue() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertThrows(IllegalArgumentException.class, () -> analyzer.getMostFrequentWords(-1),
            "Should throw IllegalArgumentException with negative value");
    }

    @Test
    public void testGetMostFrequentWordsShouldReturnListOfMostFrequentWords() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is the worst . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals("movie", analyzer.getMostFrequentWords(2).get(0),
            "Should return list of most frequent words");
        assertEquals("film", analyzer.getMostFrequentWords(2).get(1),
            "Should return list of most frequent words");
    }

    @Test
    public void testGetMostPositiveWordsShouldThrowWithNegativeValue() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertThrows(IllegalArgumentException.class, () -> analyzer.getMostPositiveWords(-1),
            "Should throw IllegalArgumentException with negative value");
    }

    @Test
    public void testGetMostPositiveWordsShouldReturnListOfMostFrequentWords() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is the worst . " +
            System.lineSeparator() + "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals("good", analyzer.getMostPositiveWords(2).get(0),
            "Should return list of most positive words");
        assertEquals("movie", analyzer.getMostPositiveWords(2).get(1),
            "Should return list of most positive words");
    }

    @Test
    public void testGetMostNegativeWordsShouldThrowWithNegativeValue() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and";
        String reviews = "2 This Movie is okay . " + System.lineSeparator() +
            "4 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is very bad . " +
            System.lineSeparator() + "1 This is a very bad movie ." + System.lineSeparator() +
            "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertThrows(IllegalArgumentException.class, () -> analyzer.getMostNegativeWords(-1),
            "Should throw IllegalArgumentException with negative value");
    }

    @Test
    public void testGetMostNegativeWordsShouldReturnListOfMostFrequentWords() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator() +
            "0 Film is the worst . " +
            System.lineSeparator() + "4 This is a good movie .";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals("worst", analyzer.getMostNegativeWords(2).get(0),
            "Should return list of most negative words");
        assertEquals("film", analyzer.getMostNegativeWords(2).get(1),
            "Should return list of most negative words");
    }

    @Test
    public void testGetSentimentDictionarySize() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(8, analyzer.getSentimentDictionarySize(), "Should return size of sentiment dictionary");
    }

    @Test
    public void testGetSentimentDictionarySizeWithEmptyReviews() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "";

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(0, analyzer.getSentimentDictionarySize(), "Should return size of sentiment dictionary");
    }

    @Test
    public void testIsStopWordShouldWorkCorrectly() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertTrue(analyzer.isStopWord("this"), "Should return true for stop word");
        assertFalse(analyzer.isStopWord("an"), "Should return false for a non stop word");
    }

    @Test
    public void testAppendReviewShouldThrowWithInvalidParameters() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertThrows(IllegalArgumentException.class, () -> analyzer.appendReview(null, 1),
            "Should throw IllegalArgumentException with null review");
        assertThrows(IllegalArgumentException.class, () -> analyzer.appendReview("    ", 1),
            "Should throw IllegalArgumentException with blank review");
        assertThrows(IllegalArgumentException.class, () -> analyzer.appendReview("", 1),
            "Should throw IllegalArgumentException with empty review");
        assertThrows(IllegalArgumentException.class, () -> analyzer.appendReview("review", 6),
            "Should throw IllegalArgumentException with invalid rating");
        assertThrows(IllegalArgumentException.class, () -> analyzer.appendReview("review", -1),
            "Should throw IllegalArgumentException with invalid rating");
    }

    @Test
    public void testAppendReviewShouldAppendTheNewReviewAndUpdateDataBase() {
        String stopWords =
            "this" + System.lineSeparator() + "is" + System.lineSeparator() + "not" + System.lineSeparator() + "test" +
                System.lineSeparator() + "and" + System.lineSeparator() + "the";
        String reviews = "3 This Movie is okay . " + System.lineSeparator() +
            "3 Film is not a masterclass but an absolute masterpiece . " + System.lineSeparator();

        MovieReviewSentimentAnalyzer analyzer = new MovieReviewSentimentAnalyzer(new StringReader(stopWords),
            new StringReader(reviews), new StringWriter(reviews.length()));

        assertEquals(0, analyzer.getWordFrequency("good"), "Should return correct value before the append");
        assertEquals(8, analyzer.getSentimentDictionarySize(), "Should return correct value before the append");
        assertEquals(3.000, analyzer.getWordSentiment("movie"), "Should return correct value before the append");
        analyzer.appendReview("This is a good movie", 4);
        assertEquals(1, analyzer.getWordFrequency("good"), "Should return correct value after the append");
        assertEquals(9, analyzer.getSentimentDictionarySize(), "Should return correct value after the append");
        assertEquals(3.500, analyzer.getWordSentiment("movie"), "Should return correct value after the append");
    }

}
