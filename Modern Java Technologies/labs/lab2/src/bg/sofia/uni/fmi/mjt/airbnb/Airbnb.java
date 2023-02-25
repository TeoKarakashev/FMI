package bg.sofia.uni.fmi.mjt.airbnb;

import bg.sofia.uni.fmi.mjt.airbnb.accommodation.Bookable;
import bg.sofia.uni.fmi.mjt.airbnb.filter.Criterion;

public class Airbnb implements AirbnbAPI {

    private Bookable[] accommodations;

    public Airbnb(Bookable[] accommodations) {
        this.accommodations = accommodations;
    }

    @Override
    public Bookable findAccommodationById(String id) {
        if(id != null) {
            for (int i = 0; i < accommodations.length; i++) {
                if (accommodations[i].getId().toLowerCase().equals(id.toLowerCase())) {
                    return accommodations[i];
                }
            }
        }
        return null;
    }

    @Override
    public double estimateTotalRevenue() {
        double sum = 0;
        for (int i = 0; i < accommodations.length; i++) {
                sum += accommodations[i].getTotalPriceOfStay();
        }
        return sum;
    }

    @Override
    public long countBookings() {
        long count = 0;
        for (int i = 0; i < accommodations.length; i++) {
            if (accommodations[i].isBooked()) {
                count++;
            }
        }
        return count;
    }

    @Override
    public Bookable[] filterAccommodations(Criterion... criteria) {
        Bookable[] filteredAccommodations = new Bookable[accommodations.length];
        int count = 0;
        for (int i = 0; i < accommodations.length; i++) {
            boolean isAccommodationMatching = true;
            for (int j = 0; j < criteria.length; j++) {
                if (!criteria[j].check(accommodations[i])) {
                    isAccommodationMatching = false;
                    break;
                }
            }
            if (isAccommodationMatching) {
                filteredAccommodations[count] = accommodations[i];
                count++;
            }
        }
        Bookable[] result = new Bookable[count];
        for (int i = 0; i < count; i++) {
            result[i] = filteredAccommodations[i];
        }
        return result;
    }
}
