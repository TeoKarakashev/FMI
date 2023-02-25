package bg.sofia.uni.fmi.mjt.sentiment.reader;

import java.io.BufferedReader;
import java.io.Reader;
import java.util.List;
import java.util.stream.Collectors;

public class StopWordsReader {

    public List<String> readStopWords(Reader stopwordsIn) {
        return new BufferedReader(stopwordsIn)
            .lines()
            .collect(Collectors.toList());
    }
}
