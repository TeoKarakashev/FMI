import bg.sofia.uni.fmi.mjt.airbnb.Airbnb;
import bg.sofia.uni.fmi.mjt.airbnb.accommodation.Apartment;
import bg.sofia.uni.fmi.mjt.airbnb.accommodation.Bookable;

public class Main {
    public static void main(String[] args) {
        Bookable app = new Apartment(null, 15);
        System.out.println(app.getId());
        Airbnb airbnb = new Airbnb(new Bookable[]{app});
        Bookable app2 = airbnb.findAccommodationById("APA-0");
        System.out.println(app2.getPricePerNight());
    }
}