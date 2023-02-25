package bg.sofia.uni.fmi.mjt.airbnb.accommodation;

import bg.sofia.uni.fmi.mjt.airbnb.accommodation.location.Location;

import java.time.Duration;
import java.time.LocalDateTime;

public abstract class Accommodation implements Bookable{

    String id;
    private Location location;
    private double pricePerNight;
    boolean isBooked;
    LocalDateTime checkIn;
    LocalDateTime checkOut;

    public Accommodation(String id, Location location, double pricePerNight, boolean isBooked, LocalDateTime checkIn, LocalDateTime checkOut) {
        this.id = id;
        this.location = location;
        this.pricePerNight = pricePerNight;
        this.isBooked = isBooked;
        this.checkIn = checkIn;
        this.checkOut = checkOut;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public Location getLocation() {
        return location;
    }

    @Override
    public boolean isBooked() {
        return isBooked;
    }

    @Override
    public boolean book(LocalDateTime checkIn, LocalDateTime checkOut) {
        if(isBooked) {
            return false;
        }
        if(checkIn == null || checkOut == null) {
            return false;
        }

        if(checkIn.isAfter(checkOut) || checkIn == checkOut) {
            return false;
        }

        if(checkIn.isBefore(LocalDateTime.now())){
            return false;
        }
        this.checkIn = checkIn;
        this.checkOut = checkOut;
        isBooked = true;
        return true;
    }

    @Override
    public double getTotalPriceOfStay() {
        if(!isBooked) {
            return 0.0;
        }
        long duration = Duration.between(checkIn, checkOut).toDays();
        return pricePerNight * duration;
    }

    @Override
    public double getPricePerNight() {
        return pricePerNight;
    }
}
