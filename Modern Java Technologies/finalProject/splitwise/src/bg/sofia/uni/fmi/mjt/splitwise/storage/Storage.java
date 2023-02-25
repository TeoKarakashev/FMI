package bg.sofia.uni.fmi.mjt.splitwise.storage;

public interface Storage {


    void addAccount(Account account);
    Account getAccount(String username);
    Account getAccount(String username, String password);
}
