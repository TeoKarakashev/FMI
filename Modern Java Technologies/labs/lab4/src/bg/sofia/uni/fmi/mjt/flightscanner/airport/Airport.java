package bg.sofia.uni.fmi.mjt.flightscanner.airport;

public record Airport(String id) {

    public Airport {
        if (id == null || id.isBlank()) {
            throw new IllegalArgumentException();
        }
    }
}

