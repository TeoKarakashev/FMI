package bg.sofia.uni.fmi.mjt.splitwise.validator;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ValidatorTest {

    @Test
    public void testValidatePasswordShouldReturnFalseWithPasswordLessThan8Characters() {
        String password = "1234567";
        assertFalse(Validator.validatePassword(password), "Password should be at least 8 characters long");
    }

    @Test
    public void testValidatePasswordShouldReturnFalseWithPasswordWithoutDigits() {
        String password = "abcdefgh";
        assertFalse(Validator.validatePassword(password), "Password should contain at least one digit");
    }

    @Test
    public void testValidatePasswordShouldReturnFalseWithPasswordWithoutCharacters() {
        String password = "12345678";
        assertFalse(Validator.validatePassword(password), "Password should contain at least one letter");
    }

    @Test
    public void testValidatePasswordShouldReturnTrueWithValidPassword() {
        String password = "12345678abcd";
        assertTrue(Validator.validatePassword(password), "Password should be valid");
    }
}
