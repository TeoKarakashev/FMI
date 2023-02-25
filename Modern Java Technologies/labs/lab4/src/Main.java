import bg.sofia.uni.fmi.mjt.flightscanner.FlightScanner;
import bg.sofia.uni.fmi.mjt.flightscanner.FlightScannerAPI;
import bg.sofia.uni.fmi.mjt.flightscanner.airport.Airport;
import bg.sofia.uni.fmi.mjt.flightscanner.exception.FlightCapacityExceededException;
import bg.sofia.uni.fmi.mjt.flightscanner.flight.Flight;
import bg.sofia.uni.fmi.mjt.flightscanner.flight.RegularFlight;
import bg.sofia.uni.fmi.mjt.flightscanner.passenger.Gender;
import bg.sofia.uni.fmi.mjt.flightscanner.passenger.Passenger;

import java.util.List;

public class Main {
    @SuppressWarnings("checkstyle:MagicNumber")
    public static void main(String[] args) throws FlightCapacityExceededException {

        FlightScanner flightScanner = new FlightScanner();
        Airport sofia = new Airport("Sofia");
        Airport budapest = new Airport("Budapest");
        Airport vien = new Airport("Vien");
        Airport manchester = new Airport("Manchester");
        Airport liverpool = new Airport("Liverpool");
        Airport london = new Airport("London");
        Airport tokyo = new Airport("Tokyo");
        Airport washington = new Airport("Washington");

        Flight flight1 = RegularFlight.of("1", budapest, sofia, 50);
        Flight flight13 = RegularFlight.of("13", sofia, vien, 50);
        Flight flight2 = RegularFlight.of("2", sofia, vien, 60);
        Flight flight3 = RegularFlight.of("3", sofia, vien, 300);
        Flight flight4 = RegularFlight.of("4", vien, manchester, 300);
        Flight flight5 = RegularFlight.of("5", manchester, sofia, 300);
        Flight flight6 = RegularFlight.of("6", manchester, sofia, 300);
        Flight flight7 = RegularFlight.of("7", manchester, vien, 300);
        Flight flight8 = RegularFlight.of("8", manchester, liverpool, 300);
        Flight flight9 = RegularFlight.of("9", liverpool, manchester, 300);
        Flight flight10 = RegularFlight.of("10", liverpool, london, 300);
        Flight flight11 = RegularFlight.of("11", liverpool, washington, 300);
        Flight flight12 = RegularFlight.of("12", washington, tokyo, 300);

        flight5.addPassenger(new Passenger("5", "Geri", Gender.FEMALE));
        flight5.addPassenger(new Passenger("5", "Beli", Gender.FEMALE));
        flight6.addPassenger(new Passenger("6", "Portokeli", Gender.FEMALE));
        flight8.addPassenger(new Passenger("8", "Veri", Gender.FEMALE));

        flightScanner.add(flight1);
        flightScanner.add(flight2);
        flightScanner.add(flight3);
        flightScanner.add(flight4);
        flightScanner.add(flight5);
        flightScanner.add(flight6);
        flightScanner.add(flight7);
        flightScanner.add(flight8);
        flightScanner.add(flight9);
        flightScanner.add(flight10);
        flightScanner.add(flight11);
        flightScanner.add(flight12);
        flightScanner.add(flight13);

        List<Flight> flights = flightScanner.searchFlights(budapest, tokyo);
        for (Flight flight : flights) {
            System.out.println(
                flight.getFrom().id() + " -> " + flight.getTo().id() + " -> " + flight.getFreeSeatsCount());
        }

    }
}