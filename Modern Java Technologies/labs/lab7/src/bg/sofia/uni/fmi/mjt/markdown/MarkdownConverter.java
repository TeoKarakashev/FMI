package bg.sofia.uni.fmi.mjt.markdown;

import java.io.*;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;

public class MarkdownConverter implements MarkdownConverterAPI {

    private LineConverterAPI lineConverter;

    public MarkdownConverter() {
        lineConverter = new LineConverter();
    }

    @Override
    public void convertMarkdown(Reader source, Writer output) {
        try (var bufferedWriter = new BufferedWriter(output)) {
            bufferedWriter.write("<html>");
            bufferedWriter.write(System.lineSeparator());
            bufferedWriter.write("<body>");
            bufferedWriter.write(System.lineSeparator());
            bufferedWriter.flush();
            try (var bufferedReader = new BufferedReader(source)) {
                String line;
                while ((line = bufferedReader.readLine()) != null) {
                    bufferedWriter.write(lineConverter.convertLine(line));
                    bufferedWriter.write(System.lineSeparator());
                    bufferedWriter.flush();
                }
                bufferedWriter.write("</body>");
                bufferedWriter.write(System.lineSeparator());
                bufferedWriter.write("</html>");
                bufferedWriter.write(System.lineSeparator());
                bufferedWriter.flush();
            } catch (IOException e) {
                throw new IllegalStateException("A problem occurred while reading from a file", e);
            }
        } catch (IOException e) {
            throw new IllegalStateException("A problem occurred while writing to a file", e);
        }

    }

    @Override
    public void convertMarkdown(Path from, Path to) {
        try (var reader = Files.newBufferedReader(from);
             var writer = Files.newBufferedWriter(to)) {
            convertMarkdown(reader, writer);
        } catch (IOException e) {
            throw new IllegalStateException("A problem occurred while reading from a file", e);
        }
    }

    @Override
    public void convertAllMarkdownFiles(Path sourceDir, Path targetDir) {
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(sourceDir, "*.md")) {
            for (Path file : stream) {
                convertMarkdown(file, targetDir.resolve(file.getFileName().toString().replace(".md", ".html")));
            }
        } catch (IOException | DirectoryIteratorException e) {
            throw new IllegalStateException("A problem occurred while loading the directory", e);
        }
    }
}
