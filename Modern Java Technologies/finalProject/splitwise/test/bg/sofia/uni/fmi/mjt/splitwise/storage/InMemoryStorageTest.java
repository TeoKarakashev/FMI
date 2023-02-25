package bg.sofia.uni.fmi.mjt.splitwise.storage;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class InMemoryStorageTest {

    @Test
    public void testGetAccountByUsernameShouldWorkCorrectly() {
        Storage storage = new InMemoryStorage();
        Account ac1 = new Account("gogo", "gogov", "gogo", "123");
        Account ac2 = new Account("pesho", "peshov", "pesho", "1232");
        storage.addAccount(ac1);
        storage.addAccount(ac2);
        assertEquals(ac1.username(), storage.getAccount("gogo").username(), "getAccount should return the correct account");
        assertEquals(ac2.username(), storage.getAccount("pesho").username(), "getAccount should return the correct account");
        assertNull(storage.getAccount("username3"), "getAccount should return null if the account does not exist");
    }

    @Test
    public void testGetAccountByUsernameAndPasswordShouldWorkCorrectly() {
        Storage storage = new InMemoryStorage();
        Account ac1 = new Account("gogo", "gogov", "gogo", "123");
        Account ac2 = new Account("pesho", "peshov", "pesho", "1232");
        storage.addAccount(ac1);
        storage.addAccount(ac2);
        assertEquals(ac1.username(), storage.getAccount("gogo", "123").username(), "getAccount should return the correct account");
        assertEquals(ac2.username(), storage.getAccount("pesho", "1232").username(), "getAccount should return the correct account");
        assertNull(storage.getAccount("gogo", "1234"), "getAccount should return null if the account does not exist");
    }
}
