package bg.sofia.uni.fmi.mjt.airbnb.accommodation;

import bg.sofia.uni.fmi.mjt.airbnb.accommodation.location.Location;

import java.time.LocalDateTime;

public class Villa extends Accommodation {
    private static int count = 0;

    public Villa (Location location, double pricePerNight) {
        super("VIL-" + count++, location, pricePerNight, false, null, null);
    }
}
