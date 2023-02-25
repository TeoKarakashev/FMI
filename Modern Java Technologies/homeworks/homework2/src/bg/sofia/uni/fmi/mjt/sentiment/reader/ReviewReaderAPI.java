package bg.sofia.uni.fmi.mjt.sentiment.reader;

import java.util.List;
import java.util.Set;

public interface ReviewReaderAPI {

    Set<String> parseReview(String review, List<String> stopWords);

    List<String> parseReviewToBeGraded(String review, List<String> stopWords);

    double getReviewScore(String review);
}
