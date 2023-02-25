public class TourGuide {

    public static int getBestSightseeingPairScore(int[] places){

        int maxScore = 0;
        for (int i = 0; i < places.length; i++) {
            for (int j = i + 1; j < places.length; j++) {
                int score = places[i] + places[j] + i - j;
                if (score > maxScore) {
                    maxScore = score;
                }
            }
        }
        return maxScore;
    }
}
