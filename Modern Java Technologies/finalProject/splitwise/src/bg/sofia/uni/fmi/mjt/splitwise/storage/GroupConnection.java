package bg.sofia.uni.fmi.mjt.splitwise.storage;

import java.util.HashMap;
import java.util.Map;

public class GroupConnection {

    private final String owner;
    private double payedByOwner;
    private final Map<String, Double> payedByGroup;

    public GroupConnection(String owner) {

        this.owner = owner;
        this.payedByOwner = 0;
        this.payedByGroup = new HashMap<>();
    }

    public GroupConnection(String owner, double payedByOwner, Map<String, Double> payedByGroup) {
        this.owner = owner;
        this.payedByOwner = payedByOwner;
        this.payedByGroup = payedByGroup;
    }

    public Map<String, Double> memberOfGroup() {
        return payedByGroup;
    }


    public double payedByOwner() {
        return payedByOwner;
    }

    public Map<String, Double> payedByGroup() {
        return payedByGroup;
    }


    public void split(double amount) {
        this.payedByOwner += (amount / (payedByGroup.size() + 1));
    }


    public void payed(String username, double amount) {
        Double prev = this.payedByGroup.get(username);
        this.payedByGroup.put(username, prev + amount);
    }

    public void addMember(String username) {
        this.payedByGroup.put(username, 0.0);
    }


    public String owner() {
        return owner;
    }

    public boolean containsUser(String username) {
        return payedByGroup.containsKey(username);
    }
}
