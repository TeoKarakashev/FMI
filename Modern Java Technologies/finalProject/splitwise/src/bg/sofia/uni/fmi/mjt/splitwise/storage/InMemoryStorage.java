package bg.sofia.uni.fmi.mjt.splitwise.storage;

import java.util.HashSet;
import java.util.Set;

public class InMemoryStorage implements Storage {
    Set<Account> accounts;


    public InMemoryStorage() {
        accounts = new HashSet<>();
    }
    @Override
    public void addAccount(Account account) {
        accounts.add(account);
    }

    @Override
    public Account getAccount(String username) {
        for (Account account : accounts) {
            if (account.username().equals(username)) {
                return account;
            }
        }
        return null;
    }

    @Override
    public Account getAccount(String username, String password) {
        for (Account account : accounts) {
            if (account.username().equals(username) && account.password().equals(password)) {
                return account;
            }
        }
        return null;
    }

}
