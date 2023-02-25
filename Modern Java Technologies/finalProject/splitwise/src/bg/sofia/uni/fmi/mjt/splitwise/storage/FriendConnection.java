package bg.sofia.uni.fmi.mjt.splitwise.storage;

public class FriendConnection {

    private double payedByOwner;
    private double payedByFriend;

    public FriendConnection() {
        this.payedByOwner = 0;
        this.payedByFriend = 0;
    }

    public FriendConnection(double payedByOwner, double payedByFriend) {
        this.payedByOwner = payedByOwner;
        this.payedByFriend = payedByFriend;
    }

    public double payedByOwner() {
        return payedByOwner;
    }

    public double payedByFriend() {
        return payedByFriend;
    }

    public void split(double amount) {
        this.payedByOwner += (amount / 2);
    }

    public void payed(double amount) {
        this.payedByFriend += amount;
    }

}
