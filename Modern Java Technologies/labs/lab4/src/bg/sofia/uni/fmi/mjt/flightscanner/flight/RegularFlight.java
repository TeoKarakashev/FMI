package bg.sofia.uni.fmi.mjt.flightscanner.flight;

import bg.sofia.uni.fmi.mjt.flightscanner.airport.Airport;
import bg.sofia.uni.fmi.mjt.flightscanner.exception.FlightCapacityExceededException;
import bg.sofia.uni.fmi.mjt.flightscanner.exception.InvalidFlightException;
import bg.sofia.uni.fmi.mjt.flightscanner.passenger.Passenger;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;

public class RegularFlight implements Flight {

    private String flightId;
    private Airport from;
    private Airport to;
    private int totalCapacity;
    private int freeSeats;
    private Collection<Passenger> passengers;


    private RegularFlight(String flightId, Airport from, Airport to, int totalCapacity) {
        if (validData(flightId, from, to, totalCapacity)) {
            this.flightId = flightId;
            this.from = from;
            this.to = to;
            this.totalCapacity = totalCapacity;
            this.freeSeats = totalCapacity;
            this.passengers = new HashSet<>();
        }
    }

    private boolean validData(String flightId, Airport from, Airport to, int totalCapacity) {
        if (flightId == null || flightId.isBlank() || from == null || to == null || totalCapacity < 0) {
            throw new IllegalArgumentException("Invalid data");
        }
        if (from.equals(to)) {
            throw new InvalidFlightException("Invalid data");
        }
        return true;
    }

    public static RegularFlight of(String flightId, Airport from, Airport to, int totalCapacity) {
        return new RegularFlight(flightId, from, to, totalCapacity);
    }

    @Override
    public Airport getFrom() {
        return from;
    }

    @Override
    public Airport getTo() {
        return to;
    }

    @Override
    public void addPassenger(Passenger passenger) throws FlightCapacityExceededException {
        if (freeSeats == 0) {
            throw new FlightCapacityExceededException("there are no free seats");
        }
        passengers.add(passenger);
        freeSeats--;
    }

    @Override
    public void addPassengers(Collection<Passenger> passengers) throws FlightCapacityExceededException {
        if (passengers.size() > freeSeats) {
            throw new FlightCapacityExceededException("the available free seats are not enough");
        }
        this.passengers.addAll(passengers);
        freeSeats -= passengers.size();
    }

    @Override
    public Collection<Passenger> getAllPassengers() {
        return Collections.unmodifiableCollection(passengers);
    }

    @Override
    public int getFreeSeatsCount() {
        return freeSeats;
    }
}
