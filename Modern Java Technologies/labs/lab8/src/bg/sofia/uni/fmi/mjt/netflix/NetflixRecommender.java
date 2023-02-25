package bg.sofia.uni.fmi.mjt.netflix;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.*;
import java.util.stream.Collectors;

public class NetflixRecommender {
    private static final int SENSITIVITY_THRESHOLD = 10_000;
    private static final String SUMMARY_COMMA_PATTERN = "[^a-zA-Z0-9]+";
    private final List<Content> content;

    /**
     * Loads the dataset from the given {@code reader}.
     *
     * @param reader Reader from which the dataset can be read.
     */
    public NetflixRecommender(Reader reader) {
        try (var bufferedReader = new BufferedReader(reader)) {
            content = bufferedReader
                .lines()
                .skip(1)
                .map(Content::of).toList();
        } catch (IOException e) {
            throw new IllegalStateException("A problem occurred while reading from the file", e);
        }
    }

    /**
     * Returns all movies and shows from the dataset in undefined order as an unmodifiable List.
     * If the dataset is empty, returns an empty List.
     *
     * @return the list of all movies and shows.
     */
    public List<Content> getAllContent() {
        return List.copyOf(content);
    }

    /**
     * Returns a list of all unique genres of movies and shows in the dataset in undefined order.
     * If the dataset is empty, returns an empty List.
     *
     * @return the list of all genres
     */
    public List<String> getAllGenres() {
        return content.stream()
            .flatMap(content -> content.genres().stream())
            .distinct()
            .toList();
    }

    /**
     * Returns the movie with the longest duration / run time. If there are two or more movies
     * with equal maximum run time, returns any of them. Shows in the dataset are not considered by this method.
     *
     * @return the movie with the longest run time
     * @throws NoSuchElementException in case there are no movies in the dataset.
     */
    public Content getTheLongestMovie() {
        return content.stream()
            .filter(c -> c.type().equals(ContentType.MOVIE))
            .max(Comparator.comparing(Content::runtime))
            .orElseThrow(() -> new NoSuchElementException("no movies in the database"));
    }

    /**
     * Returns a breakdown of content by type (movie or show).
     *
     * @return a Map with key: a ContentType and value: the set of movies or shows on the dataset, in undefined order.
     */
    public Map<ContentType, Set<Content>> groupContentByType() {
        return content.stream()
            .collect(Collectors.groupingBy(Content::type, Collectors.toSet()));
    }

    /**
     * Returns the top N movies and shows sorted by weighed IMDB rating in descending order.
     * If there are fewer movies and shows than {@code n} in the dataset, return all of them.
     * If {@code n} is zero, returns an empty list.
     * <p>
     * The weighed rating is calculated by the following formula:
     * Weighted Rating (WR) = (v ÷ (v + m)) × R + (m ÷ (v + m)) × C
     * where
     * R is the content's own average rating across all votes. If it has no votes, its R is 0.
     * C is the average rating of content across the dataset
     * v is the number of votes for a content
     * m is a tunable parameter: sensitivity threshold. In our algorithm, it's a constant equal to 10_000.
     * <p>
     * Check https://stackoverflow.com/questions/1411199/what-is-a-better-way-to-sort-by-a-5-star-rating for details.
     *
     * @param n the number of the top-rated movies and shows to return
     * @return the list of the top-rated movies and shows
     * @throws IllegalArgumentException if {@code n} is negative.
     */
    public List<Content> getTopNRatedContent(int n) {

        if (n < 0) {
            throw new IllegalArgumentException("n must be positive");
        }

        double averageRatingOfTheDataset = content.stream().mapToDouble(Content::imdbScore).average().orElse(0);
        return content.stream()
            .sorted((c1, c2) -> Double.compare(getWeightedRating(c2, averageRatingOfTheDataset),
                getWeightedRating(c1, averageRatingOfTheDataset)))
            .limit(n)
            .toList();
    }

    // Weighted Rating (WR) = (v ÷ (v + m)) × R + (m ÷ (v + m)) × C
    //     * where
    //     * R is the content's own average rating across all votes. If it has no votes, its R is 0.
    //     * C is the average rating of content across the dataset
    //     * v is the number of votes for a content
    //     * m is a tunable parameter: sensitivity threshold. In our algorithm, it's a constant equal to 10_000.
    private double getWeightedRating(Content content, double c) {
        double v = content.imdbVotes();
        double m = SENSITIVITY_THRESHOLD;
        double r = content.imdbScore();
        return (v / (v + m)) * r + (m / (v + m)) * c;
    }

    /**
     * Returns a list of content similar to the specified one sorted by similarity is descending order.
     * Two contents are considered similar, only if they are of the same type (movie or show).
     * The used measure of similarity is the number of genres two contents share.
     * If two contents have equal number of common genres with the specified one, their mutual oder
     * in the result is undefined.
     *
     * @param content the specified movie or show.
     * @return the sorted list of content similar to the specified one.
     */
    public List<Content> getSimilarContent(Content content) {

        return this.content.stream()
            .filter(c -> c.type().equals(content.type()))
            .sorted(
                (c1, c2) -> Integer.compare(getNumberOfCommonGenres(c2, content), getNumberOfCommonGenres(c1, content)))
            .toList();
    }

    private int getNumberOfCommonGenres(Content c1, Content c2) {
        return (int) c1.genres().stream().filter(c2.genres()::contains).count();
    }

    /**
     * Searches content by keywords in the description (case-insensitive).
     *
     * @param keywords the keywords to search for
     * @return an unmodifiable set of movies and shows whose description contains all specified keywords.
     */
    public Set<Content> getContentByKeywords(String... keywords) {
        List<String> keywordsList = Arrays.stream(keywords).map(String::toLowerCase).toList();

        return content.stream()
            .filter(c -> Arrays.stream(c.description().split(SUMMARY_COMMA_PATTERN))
                .map(String::toLowerCase)
                .collect(Collectors.toSet()).containsAll(keywordsList))
            .collect(Collectors.toUnmodifiableSet());
    }

}