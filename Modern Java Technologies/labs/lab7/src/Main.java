import bg.sofia.uni.fmi.mjt.markdown.MarkdownConverter;
import bg.sofia.uni.fmi.mjt.markdown.MarkdownConverterAPI;

import java.nio.file.Path;

public class Main {
    public static void main(String[] args) {

        MarkdownConverterAPI markdownConverterAPI = new MarkdownConverter();
        markdownConverterAPI.convertAllMarkdownFiles(
            Path.of("C:\\Users\\teodor\\OneDrive\\Documents\\GitHub\\Java\\lab7\\testFiles"),
            Path.of("C:\\Users\\teodor\\OneDrive\\Documents\\GitHub\\Java\\lab7\\test2"));
    }
}