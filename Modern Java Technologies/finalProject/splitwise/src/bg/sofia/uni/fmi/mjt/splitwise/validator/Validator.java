package bg.sofia.uni.fmi.mjt.splitwise.validator;

public class Validator {

    private static final int MIN_PASSWORD_LENGTH = 8;

    public static boolean validatePassword(String password) {
        boolean containsDigit = false;
        boolean containsLetter = false;

        if (password.length() < MIN_PASSWORD_LENGTH) {
            return false;
        }

        for (int i = 0; i < password.length(); i++) {
            if (Character.isDigit(password.charAt(i))) {
                containsDigit = true;
            }
            if (Character.isLetter(password.charAt(i))) {
                containsLetter = true;
            }
        }

        return containsDigit && containsLetter;
    }
}
