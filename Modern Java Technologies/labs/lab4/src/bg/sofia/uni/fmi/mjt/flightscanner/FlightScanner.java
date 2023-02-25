package bg.sofia.uni.fmi.mjt.flightscanner;

import bg.sofia.uni.fmi.mjt.flightscanner.airport.Airport;
import bg.sofia.uni.fmi.mjt.flightscanner.flight.Flight;
import bg.sofia.uni.fmi.mjt.flightscanner.flight.FlightByDestinationComparator;
import bg.sofia.uni.fmi.mjt.flightscanner.flight.FlightByFreeSeatsComparator;

import java.util.*;

public class FlightScanner implements FlightScannerAPI {

    private Collection<Flight> flights;

    private boolean isValidFlight(Flight flight) {
        if (flight == null) {
            throw new IllegalArgumentException("Invalid flight");
        }
        return true;
    }

    public FlightScanner() {
        flights = new HashSet<>();
    }


    @Override
    public void add(Flight flight) {
        if (isValidFlight(flight)) {
            flights.add(flight);
        }
    }

    @Override
    public void addAll(Collection<Flight> flights) {
        if (flights == null) {
            throw new IllegalArgumentException("Invalid flights");
        }
        Collections.addAll(flights);
    }

    @Override
    public List<Flight> searchFlights(Airport from, Airport to) {
        isValidInput(from, to);

        return findShortestPath(from, to);
    }

    private List<Flight> findShortestPath(Airport from, Airport to) {
        List<Flight> result = new LinkedList<>();
        Queue<Airport> queue = new LinkedList<>();
        Set<Airport> visited = new HashSet<>();
        Map<Airport, Airport> previous = new HashMap<>();
        Set<Airport> neighbours = new HashSet<>();

        visited.add(from);
        queue.add(from);
        while (!queue.isEmpty()) {
            Airport current = queue.poll();
            for (Flight flight : flights) {
                if (flight.getFrom().equals(current)) {
                    neighbours.add(flight.getTo());
                }
            }
            for (Airport neighbour : neighbours) {
                if (!visited.contains(neighbour)) {
                    visited.add(neighbour);
                    previous.put(neighbour, current);
                    queue.add(neighbour);
                }
            }
        }

        Airport current = to;
        if (previous.containsKey(to)) {
            while (!current.equals(from)) {
                result.add(findFlight(previous.get(current), current));
                current = previous.get(current);
            }
            Collections.reverse(result);
            return result;
        }
        return Collections.emptyList();
    }

    private Flight findFlight(Airport from, Airport to) {
        for (Flight flight : flights) {
            if (flight.getFrom().equals(from) && flight.getTo().equals(to)) {
                return flight;
            }
        }
        return null;
    }

    @Override
    public List<Flight> getFlightsSortedByFreeSeats(Airport from) {
        isValidInput(from);

        List<Flight> toReturn = new ArrayList<>();

        for (Flight flight : flights) {
            if (flight.getFrom().equals(from)) {
                toReturn.add(flight);
            }
        }

        toReturn.sort(new FlightByFreeSeatsComparator());
        return Collections.unmodifiableList(toReturn);
    }

    @Override
    public List<Flight> getFlightsSortedByDestination(Airport from) {
        isValidInput(from);

        List<Flight> toReturn = new ArrayList<>();

        for (Flight flight : flights) {
            if (flight.getFrom().equals(from)) {
                toReturn.add(flight);
            }
        }
        toReturn.sort(new FlightByDestinationComparator());
        return Collections.unmodifiableList(toReturn);
    }

    private boolean isValidInput(Airport from) {
        if (from == null) {
            throw new IllegalArgumentException("Invalid airport");
        }
        return true;
    }

    private boolean isValidInput(Airport from, Airport to) {
        if (from == null || to == null || from.equals(to)) {
            throw new IllegalArgumentException("Invalid airport");
        }
        return true;
    }

    //create to string

}
