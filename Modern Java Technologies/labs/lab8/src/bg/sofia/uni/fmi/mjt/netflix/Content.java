package bg.sofia.uni.fmi.mjt.netflix;

import java.util.Arrays;
import java.util.List;

public record Content(String id, String title, ContentType type, String description, int releaseYear,
                      int runtime, List<String> genres, int seasons, String imdbId, double imdbScore,
                      double imdbVotes) {
    //id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes
    private static final String CONTENT_ATTRIBUTE_DELIMITER = ",";
    private static final int CONTENT_ID = 0;
    private static final int CONTENT_TITLE = 1;
    private static final int CONTENT_TYPE = 2;
    private static final int CONTENT_DESCRIPTION = 3;
    private static final int CONTENT_RELEASE_YEAR = 4;
    private static final int CONTENT_RUNTIME = 5;
    private static final int CONTENT_GENRES = 6;
    private static final int CONTENT_SEASONS = 7;
    private static final int CONTENT_IMDB_ID = 8;
    private static final int CONTENT_IMDB_SCORE = 9;
    private static final int CONTENT_IMDB_VOTES = 10;
    private static final String CONTENT_GENRES_DELIMITER = "[^a-zA-Z]+";

    //['drama'; 'action'; 'thriller'; 'european']
    public static Content of(String line) {
        final String[] tokens = line.split(CONTENT_ATTRIBUTE_DELIMITER);
        return new Content(tokens[CONTENT_ID], tokens[CONTENT_TITLE],
            ContentType.valueOf(tokens[CONTENT_TYPE].toUpperCase()),
            tokens[CONTENT_DESCRIPTION], Integer.parseInt(tokens[CONTENT_RELEASE_YEAR]),
            Integer.parseInt(tokens[CONTENT_RUNTIME]),
            Arrays.stream(tokens[CONTENT_GENRES].split(CONTENT_GENRES_DELIMITER)).skip(1).toList(),
            Integer.parseInt(tokens[CONTENT_SEASONS]), tokens[CONTENT_IMDB_ID],
            Double.parseDouble(tokens[CONTENT_IMDB_SCORE]), Double.parseDouble(tokens[CONTENT_IMDB_VOTES]));
    }
}
