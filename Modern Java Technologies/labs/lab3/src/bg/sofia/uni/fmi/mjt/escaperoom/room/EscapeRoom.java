package bg.sofia.uni.fmi.mjt.escaperoom.room;

import bg.sofia.uni.fmi.mjt.escaperoom.rating.Ratable;

public class EscapeRoom implements Ratable {

    private String name;
    private int maxCapacity;
    private int timeForEscape;
    private Theme theme;
    private Review[] reviews;
    private int maxReviewsCount;
    private int numberOfReviews;
    private double rating;
    private double priceToPlay;
    private int maxTimeToEscape;
    private Difficulty difficulty;
    private int oldestReviewIndex;


    public EscapeRoom(String name, Theme theme, Difficulty difficulty, int maxTimeToEscape, double priceToPlay,
                      int maxReviewsCount){
        this.name = name;
        this.theme = theme;
        this.difficulty = difficulty;
        this.maxTimeToEscape = maxTimeToEscape;
        this.maxReviewsCount = maxReviewsCount;
        this.reviews = new Review[maxReviewsCount];
        this.numberOfReviews = 0;
        this.rating = 0.0;
        this.priceToPlay = priceToPlay;
        this.oldestReviewIndex = 0;

    }

    @Override
    public double getRating() {
        return rating;
    }

    /**
     * Returns the name of the escape room.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the difficulty of the escape room.
     */
    public Difficulty getDifficulty() {
        return difficulty;
    }

    /**
     * Returns the maximum time to escape the room.
     */
    public int getMaxTimeToEscape() {
        return maxTimeToEscape;
    }

    /**
     * Returns all user reviews stored for this escape room, in the order they have been added.
     */
    public Review[] getReviews() {
        Review[] allReviews;
        if(numberOfReviews >= maxReviewsCount){
            allReviews = new Review[maxReviewsCount];
            for(int i = 0; i < maxReviewsCount; i++){
                allReviews[i] = this.reviews[i];
            }
        }else{
            allReviews = new Review[numberOfReviews];
            for (int i = 0; i < numberOfReviews; i++) {
                allReviews[i] = this.reviews[i];
            }
        }
        return allReviews;
    }

    /**
     * Adds a user review for this escape room.
     * The platform keeps just the latest up to {@code maxReviewsCount} reviews and in case the capacity is full,
     * a newly added review would overwrite the oldest added one, so the platform contains
     * {@code maxReviewsCount} at maximum, at any given time. Note that, despite older reviews may have been
     * overwritten, the rating of the room averages all submitted review ratings, regardless of whether all reviews
     * themselves are still stored in the platform.
     *
     * @param review the user review to add.
     */
    public void addReview(Review review) {
        if (numberOfReviews < maxReviewsCount - 1) {
            reviews[numberOfReviews] = review;
        } else {
            reviews[oldestReviewIndex] = review;
            oldestReviewIndex = (oldestReviewIndex + 1) % maxReviewsCount;
        }
        numberOfReviews++;
        rating = (rating * (numberOfReviews - 1) + review.rating()) / numberOfReviews;
    }
}
