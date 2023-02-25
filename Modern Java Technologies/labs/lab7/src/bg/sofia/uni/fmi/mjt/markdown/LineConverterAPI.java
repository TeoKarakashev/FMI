package bg.sofia.uni.fmi.mjt.markdown;

public interface LineConverterAPI {

    /**
     * Converts a line in markdown format to a line in corresponding HTML format.
     *
     * @param line the line in markdown format
     * @return the line in HTML format
     */
    String convertLine(String line);
}
