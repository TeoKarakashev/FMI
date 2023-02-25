package bg.sofia.uni.fmi.mjt.flightscanner.passenger;

public record Passenger(String id, String name, Gender gender) {

    private static final int MAX_NAME_LENGTH = 200;

    public Passenger {
        if (id == null || id.isBlank()) {
            throw new IllegalArgumentException();
        }
        if (name == null || name.length() > MAX_NAME_LENGTH) {
            throw new IllegalArgumentException();
        }
    }
}
