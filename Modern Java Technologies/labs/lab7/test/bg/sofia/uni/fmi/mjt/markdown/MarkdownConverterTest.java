package bg.sofia.uni.fmi.mjt.markdown;


import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;


public class MarkdownConverterTest {

    @Test
    public void testLineConverterShouldWorkCorrectly() {

        LineConverterAPI lineConverter = new LineConverter();


        assertEquals(lineConverter.convertLine("# Header"), "<h1>Header</h1>", "Header1 should be converted correctly");
        assertEquals(lineConverter.convertLine("## Header"), "<h2>Header</h2>",
            "Header2 should be converted correctly");
        assertEquals(lineConverter.convertLine("### Header"), "<h3>Header</h3>",
            "Header3 should be converted correctly");
        assertEquals(lineConverter.convertLine("#### Header"), "<h4>Header</h4>",
            "Header4 should be converted correctly");
        assertEquals(lineConverter.convertLine("##### Header"), "<h5>Header</h5>",
            "Header5 should be converted correctly");
        assertEquals(lineConverter.convertLine("###### Header"), "<h6>Header</h6>",
            "Header6 should be converted correctly");
        assertEquals(lineConverter.convertLine("**Header**"), "<strong>Header</strong>",
            "Bold should be converted correctly");
        assertEquals(lineConverter.convertLine("*Header*"), "<em>Header</em>", "Italic should be converted correctly");
        assertEquals(lineConverter.convertLine("`Header`"), "<code>Header</code>",
            "Code should be converted correctly");
        assertEquals(lineConverter.convertLine("Header"), "Header", "Normal text should be converted correctly");

    }

    @Test
    public void testMarkdownConverterShouldWorkCorrectly() {

        File source = new File("tempFile.md");
        String text = "# test";
        try {
            Files.writeString(source.toPath(), text);
        } catch (IOException e) {
            e.printStackTrace();
        }
        File destination = new File("tempFile.html");

        MarkdownConverterAPI markdownConverter = new MarkdownConverter();
        markdownConverter.convertMarkdown(source.toPath(), destination.toPath());
        try (var reader = Files.newBufferedReader(destination.toPath())) {
            assertEquals(reader.readLine(), "<html>", "File should start with <html>");
            assertEquals(reader.readLine(), "<body>", "File should start with <body>");
            assertEquals(reader.readLine(), "<h1>test</h1>", "File should contain <h1>test</h1>");
            assertEquals(reader.readLine(), "</body>", "File should end with </body>");
            assertEquals(reader.readLine(), "</html>", "File should end with </html>");
        } catch (IOException e) {
            e.printStackTrace();
        }

        source.delete();
        destination.delete();
    }

    @Test
    public void testMarkdownConverterShouldThrownWhenFileNotFound() {
        MarkdownConverterAPI markdownConverter = new MarkdownConverter();
        assertThrows(IllegalStateException.class,
            () -> markdownConverter.convertMarkdown(new File("doesntExist.md").toPath(),
                new File("doesntExist.html").toPath()), "Should throw exception when file not found");
    }

    @Test
    public void testConvertAllMarkdownFilesShouldWorkCorrectly() {

        try {
            Files.createDirectory(new File("tempIn").toPath());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        //create output directory
        try {
            Files.createDirectory(new File("tempOut").toPath());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        File source1 = new File("tempIn/tempFile1.md");
        File source2 = new File("tempIn/tempFile2.md");
        File destination1 = new File("tempOut/tempFile1.html");
        File destination2 = new File("tempOut/tempFile2.html");
        String text1 = "# test1";
        String text2 = "# test2";

        try {
            Files.writeString(source1.toPath(), text1);
            Files.writeString(source2.toPath(), text2);
        } catch (IOException e) {
            e.printStackTrace();
        }

        MarkdownConverterAPI markdownConverter = new MarkdownConverter();
        markdownConverter.convertAllMarkdownFiles(Path.of("tempIn"), Path.of("tempOut"));


        assertTrue(destination1.exists(), "File should be created");
        assertTrue(destination2.exists(), "File should be created");


        try (var reader = Files.newBufferedReader(destination1.toPath())) {
            assertEquals(reader.readLine(), "<html>", "File should start with <html>");
            assertEquals(reader.readLine(), "<body>", "File should start with <body>");
            assertEquals(reader.readLine(), "<h1>test1</h1>", "File should contain <h1>test1</h1>");
            assertEquals(reader.readLine(), "</body>", "File should end with </body>");
            assertEquals(reader.readLine(), "</html>", "File should end with </html>");
        } catch (IOException e) {
            e.printStackTrace();
        }

        try (var reader = Files.newBufferedReader(destination2.toPath())) {
            assertEquals(reader.readLine(), "<html>", "File should start with <html>");
            assertEquals(reader.readLine(), "<body>", "File should start with <body>");
            assertEquals(reader.readLine(), "<h1>test2</h1>", "File should contain <h1>test2</h1>");
            assertEquals(reader.readLine(), "</body>", "File should end with </body>");
            assertEquals(reader.readLine(), "</html>", "File should end with </html>");
        } catch (IOException e) {
            e.printStackTrace();
        }

        source1.delete();
        source2.delete();
        destination1.delete();
        destination2.delete();

        new File("tempIn").delete();
        new File("tempOut").delete();
    }
}

