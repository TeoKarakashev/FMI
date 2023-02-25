package bg.sofia.uni.fmi.mjt.airbnb.accommodation;

import bg.sofia.uni.fmi.mjt.airbnb.accommodation.location.Location;

import java.time.LocalDateTime;

public class Hotel extends Accommodation {
    private static int count = 0;

    public Hotel(Location location, double pricePerNight) {
        super("HOT-" + count++, location, pricePerNight, false, null, null);
    }
}
