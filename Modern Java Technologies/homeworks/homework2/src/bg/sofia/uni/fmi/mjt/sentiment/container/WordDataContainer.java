package bg.sofia.uni.fmi.mjt.sentiment.container;

public class WordDataContainer {

    private double sentiment;
    private int frequency;

    public WordDataContainer(double sentiment) {
        this.sentiment = sentiment;
        this.frequency = 1;
    }

    private double calculateSentiment(double newSentiment) {
        return (sentiment * frequency + newSentiment) / (frequency + 1);
    }

    public int getFrequency() {
        return frequency;
    }

    public void addSentiment(double newSentiment) {
        this.sentiment = calculateSentiment(newSentiment);
        this.frequency++;
    }

    public double getSentiment() {
        return sentiment;
    }

}
