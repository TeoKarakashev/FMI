package bg.sofia.uni.fmi.mjt.netflix;

import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.*;

public class NetflixRecommenderTest {


    @Test
    public void testParsingData() {
        String content = "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
            System.lineSeparator() +
            "tm120801,The Dirty Dozen,MOVIE,Test descr,1967,150,['war'; 'action'],-1,tt0061578,7.7,72662.0";
        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllContent().get(0).id(), "tm120801", "problem with parsing id");
        assertEquals(netflixRecommender.getAllContent().get(0).title(), "The Dirty Dozen",
            "problem with parsing title");
        assertEquals(netflixRecommender.getAllContent().get(0).type(), ContentType.MOVIE, "problem with parsing type");
        assertEquals(netflixRecommender.getAllContent().get(0).description(), "Test descr",
            "problem with parsing description");
        assertEquals(netflixRecommender.getAllContent().get(0).releaseYear(), 1967, "problem with parsing releaseYear");
        assertEquals(netflixRecommender.getAllContent().get(0).runtime(), 150, "problem with parsing runtime");
        assertEquals(netflixRecommender.getAllContent().get(0).genres().get(0), "war", "problem with parsing genres");
        assertEquals(netflixRecommender.getAllContent().get(0).seasons(), -1, "problem with parsing seasons");
        assertEquals(netflixRecommender.getAllContent().get(0).imdbId(), "tt0061578", "problem with parsing imdbId");
        assertEquals(netflixRecommender.getAllContent().get(0).imdbScore(), 7.7, "problem with parsing imdbScore");
        assertEquals(netflixRecommender.getAllContent().get(0).imdbVotes(), 72662.0, "problem with parsing imdbVotes");
    }

    @Test
    public void testRetrievingAllContentShouldWorkCorrectly() {
        String content = "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
            System.lineSeparator() +
            "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran works as a night-time taxi driver " +
            "in New York City where the perceived decadence and sleaze feed his urge for violent action.,1976,114," +
            "['drama'; 'crime'],-1,tt0075314,8.2,808582.0" +
            System.lineSeparator() +
            "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River before it's " +
            "turned into one huge lake; outdoor fanatic Lewis Medlock takes his friends on a river-rafting trip they'll" +
            " never forget into the dangerous American back-country.,1972,109," +
            "['drama'; 'action'; 'thriller'; 'european'],-1,tt0068473,7.7,107673.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllContent().size(), 2, "Problem with parsing content");
    }

    @Test
    public void testRetrievingAllContentShouldBeEmptyWithJustFirstRowOfData() {
        String content = "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllContent().size(), 0, "Problem with parsing empty content");
    }

    @Test
    public void testRetrievingAllContentShouldBeEmptyWithNoData() {
        String content = "";
        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllContent().size(), 0, "Problem with parsing empty content");
    }

    @Test
    public void testGetAllGenresShouldWorkCorrectlyWithRepeatingGenres() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran works" +
                " as a night-time taxi driver in New York City where the perceived decadence and" +
                " sleaze feed his urge for violent action.,1976,114,['drama'; 'crime']," +
                "-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River before" +
                " it's turned into one huge lake; outdoor fanatic Lewis Medlock takes his friends " +
                "on a river-rafting trip they'll never forget into the dangerous American back-country." +
                ",1972,109,['drama'; 'action'; 'thriller'; 'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King " +
                "Arthur; accompanied by his squire; recruits his Knights of the Round " +
                "Table; including Sir Bedevere the Wise; Sir Lancelot the Brave; Sir Robin" +
                " the Not-Quite-So-Brave-As-Sir-Lancelot and Sir Galahad the Pure. On the way;" +
                " Arthur battles the Black Knight who; despite having had all his limbs chopped " +
                "off; insists he can still fight. They reach Camelot; but Arthur decides not  " +
                "to enter; as \"\"it is a silly place\"\".\",1975,91,['fantasy'; 'action'; " +
                "'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,MOVIE,12 American military " +
                "prisoners in World War II are ordered to infiltrate " +
                "a well-guarded enemy château and kill the Nazi officers " +
                "vacationing there. The soldiers; most of whom are facing death sentences " +
                "for a variety of violent crimes; agree to the mission and the possible commuting of their sentences." +
                ",1967,150,['war'; 'action'],-1,tt0061578,7.7,72662.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllGenres().size(), 8, "Unexpected number of genres");
    }

    @Test
    public void testGetAllGenresShouldWorkCorrectlyWithNoGenres() {
        String content = "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllGenres().size(), 0, "No genres should be present with no content");
    }

    @Test
    public void testGetAllGenresShouldWorkCorrectlyWithContentWithNoGenres() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War " +
                "veteran works as a night-time taxi driver in New York City where the perceived " +
                "decadence and sleaze feed his urge for violent action.,1976,114,[],-1,tt0075314,8.2,808582.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getAllGenres().size(), 0, "No genres should be present with no content");
    }

    @Test
    public void testGetTheLongestMovieShouldThrowWithNoContent() {
        String content = "";
        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertThrows(NoSuchElementException.class, netflixRecommender::getTheLongestMovie,
            "Should throw with no content");
    }

    @Test
    public void testGetTheLongestMovieShouldWorkCorrectlyWithLongerShow() {
        String content =
            "id,title,type,description,release_year," +
                "runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam " +
                "War veteran works as a night-time taxi driver in New York City " +
                "where the perceived decadence and sleaze feed his urge for violent " +
                "action.,1976,114,['drama'; 'crime'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River " +
                "before it's turned into one huge lake; outdoor fanatic Lewis Medlock" +
                " takes his friends on a river-rafting trip they'll never forget into " +
                "the dangerous American back-country.,1972,109,['drama'; 'action'; 'thriller'; " +
                "'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; " +
                "accompanied by his squire; recruits his Knights of the Round Table; including " +
                "Sir Bedevere the Wise; Sir Lancelot the Brave; Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot" +
                " and Sir Galahad the Pure. On the way; Arthur battles the Black Knight who; despite having had all his " +
                "limbs chopped off; insists he can still fight. They reach Camelot; but Arthur decides not  to enter; as" +
                " \"\"it is a silly place\"\".\",1975,91,['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,SHOW,12 American military prisoners " +
                "in World War II are ordered to infiltrate a well-guarded enemy château " +
                "and kill the Nazi officers vacationing there. The soldiers; most of whom are " +
                "facing death sentences for a variety of violent crimes; agree to the mission and" +
                " the possible commuting of their sentences.,1967,200,['war'; 'action'],2,tt0061578,7.7,72662.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        Content toCheck = Content.of(
            "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam " +
                "War veteran works as a night-time taxi driver in New York City where " +
                "the perceived decadence and sleaze feed his urge for violent action.," +
                "1976,114,['drama'; 'crime'],-1,tt0075314,8.2,808582.0");
        assertEquals(netflixRecommender.getTheLongestMovie(), toCheck, "Problem with getting the longest movie");
    }

    @Test
    public void testGroupContentByTypeShouldWorkCorrectly() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam " +
                "War veteran works as a night-time taxi driver in New York " +
                "City where the perceived decadence and sleaze feed his urge " +
                "for violent action.,1976,114,['drama'; 'crime'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee" +
                " River before it's turned into one huge lake; outdoor fanatic Lewis" +
                " Medlock takes his friends on a river-rafting trip they'll never forget" +
                " into the dangerous American back-country.,1972,109,['drama'; 'action';" +
                " 'thriller'; 'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; " +
                "accompanied by his squire; recruits his Knights of the Round Table;" +
                " including Sir Bedevere the Wise; Sir Lancelot the Brave; Sir Robin " +
                "the Not-Quite-So-Brave-As-Sir-Lancelot and Sir Galahad the Pure. On " +
                "the way; Arthur battles the Black Knight who; despite having had all " +
                "his limbs chopped off; insists he can still fight. They reach Camelot;" +
                " but Arthur decides not  to enter; as \"\"it is a silly place\"\".\"," +
                "1975,91,['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,SHOW,12 American military prisoners in" +
                " World War II are ordered to infiltrate a well-guarded enemy château" +
                " and kill the Nazi officers vacationing there. The soldiers; most of " +
                "whom are facing death sentences for a variety of violent crimes; agree" +
                " to the mission and the possible commuting of their sentences.,1967,200," +
                "['war'; 'action'],2,tt0061578,7.7,72662.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.groupContentByType().get(ContentType.MOVIE).size(), 3,
            "Problem with grouping movies");
        assertEquals(netflixRecommender.groupContentByType().get(ContentType.SHOW).size(), 1,
            "Problem with grouping shows");
    }

    @Test
    public void testGetTopNRatedContentShouldThrowsWithNegativeInput() {
        String content = "";
        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertThrows(IllegalArgumentException.class, () -> netflixRecommender.getTopNRatedContent(-1));
    }

    @Test
    public void testGetTopNRatedContentLimitingShouldWorkCorrectly() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran " +
                "works as a night-time taxi driver in New York City where the perceived" +
                " decadence and sleaze feed his urge for violent action.,1976,114," +
                "['drama'; 'crime'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River " +
                "before it's turned into one huge lake; outdoor fanatic Lewis Medlock" +
                " takes his friends on a river-rafting trip they'll never forget into " +
                "the dangerous American back-country.,1972,109,['drama'; 'action'; " +
                "'thriller'; 'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; accompanied" +
                " by his squire; recruits his Knights of the Round Table; including Sir" +
                " Bedevere the Wise; Sir Lancelot the Brave; Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot" +
                " and Sir Galahad the Pure. On the way; Arthur battles the Black Knight who; despite " +
                "having had all his limbs chopped off; insists he can still fight. They reach Camelot;" +
                " but Arthur decides not  to enter; as \"\"it is a silly place\"\".\",1975,91,['fantasy'; " +
                "'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,SHOW,12 American military prisoners in World War" +
                " II are ordered to infiltrate a well-guarded enemy château and kill the Nazi" +
                " officers vacationing there. The soldiers; most of whom are facing death " +
                "sentences for a variety of violent crimes; agree to the mission and the " +
                "possible commuting of their sentences.,1967,200,['war'; 'action'],2,tt0061578,7.7,72662.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getTopNRatedContent(2).size(), 2,
            "Problem With limiting the data size");
        assertEquals(netflixRecommender.getTopNRatedContent(10).size(), 4,
            "Problem With limiting the data size");
    }

    @Test
    public void testGetTopNRatedContentShouldWorkCorrectly() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran " +
                "works as a night-time taxi driver in New York City where the perceived " +
                "decadence and sleaze feed his urge for violent action.,1976,114," +
                "['drama'; 'crime'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River" +
                " before it's turned into one huge lake; outdoor fanatic Lewis Medlock " +
                "takes his friends on a river-rafting trip they'll never forget into the" +
                " dangerous American back-country.,1972,109,['drama'; 'action'; 'war';" +
                " 'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; accompanied" +
                " by his squire; recruits his Knights of the Round Table; including Sir" +
                " Bedevere the Wise; Sir Lancelot the Brave; Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot" +
                " and Sir Galahad the Pure. On the way; Arthur battles the Black Knight who;" +
                " despite having had all his limbs chopped off; insists he can still fight. " +
                "They reach Camelot; but Arthur decides not  to enter; as \"\"it is a silly " +
                "place\"\".\",1975,91,['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,SHOW,12 American military prisoners in World War " +
                "II are ordered to infiltrate a well-guarded enemy château and kill the Nazi " +
                "officers vacationing there. The soldiers; most of whom are facing death sentences " +
                "for a variety of violent crimes; agree to the mission and the possible commuting " +
                "of their sentences.,1967,200,['war'; 'action'],2,tt0061578,7.7,72662.0";

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getTopNRatedContent(2).get(0).id(), "tm84618");
        assertEquals(netflixRecommender.getTopNRatedContent(2).get(1).id(), "tm127384");
    }

    @Test
    public void testGetSimilarContentShouldWorkCorrectly() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran works " +
                "as a night-time taxi driver in New York City where the perceived decadence" +
                " and sleaze feed his urge for violent action.,1976,114,['drama'; 'action']" +
                ",-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River before " +
                "it's turned into one huge lake; outdoor fanatic Lewis Medlock takes his " +
                "friends on a river-rafting trip they'll never forget into the dangerous " +
                "American back-country.,1972,109,['drama'; 'action'; 'war'; 'european']," +
                "-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; accompanied " +
                "by his squire; recruits his Knights of the Round Table; including Sir Bedevere" +
                " the Wise; Sir Lancelot the Brave; Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot" +
                " and Sir Galahad the Pure. On the way; Arthur battles the Black Knight who; despite" +
                " having had all his limbs chopped off; insists he can still fight. They reach Camelot;" +
                " but Arthur decides not  to enter; as \"\"it is a silly place\"\".\",1975,91,['fantasy';" +
                " 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm120801,The Dirty Dozen,SHOW,12 American military prisoners in World War II are" +
                " ordered to infiltrate a well-guarded enemy château and kill the Nazi officers " +
                "vacationing there. The soldiers; most of whom are facing death sentences for a " +
                "variety of violent crimes; agree to the mission and the possible commuting of their" +
                " sentences.,1967,200,['war'; 'action'],2,tt0061578,7.7,72662.0";


        Content toCheck = Content.of(
            "tm44204,The Guns of Navarone,MOVIE,A team of allied saboteurs are " +
                "assigned an impossible mission: infiltrate an impregnable Nazi-held " +
                "island and destroy the two enormous long-range field guns that prevent" +
                " the rescue of 2;000 trapped British soldiers.,1961,158,['action'; 'drama';" +
                " 'war'],-1,tt0054953,7.5,50748.0");

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));
        assertEquals(netflixRecommender.getSimilarContent(toCheck).get(0).id(), "tm154986");
        assertEquals(netflixRecommender.getSimilarContent(toCheck).get(1).id(), "tm84618");
        assertEquals(netflixRecommender.getSimilarContent(toCheck).get(2).id(), "tm127384");
    }

    @Test
    public void testGetContentByKeywordsShouldWorkCorrectlyWithOneWord() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War " +
                "veteran works as a night-time taxi driver in New York City" +
                " where the perceived decadence and sleaze feed his urge for" +
                " violent action.,1976,114,['drama'; 'action'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee " +
                "River before it's turned into one huge lake; outdoor fanatic " +
                "Lewis Medlock takes his friends on a river-rafting trip they'll" +
                " never forget into the dangerous American back-country.,1972,109," +
                "['drama'; 'action'; 'war'; 'european'],-1,tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; " +
                "accompanied by his squire; recruits his Knights of the Round " +
                "Table; including Sir Bedevere the Wise; Sir Lancelot the Brave;" +
                " Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot and Sir Galahad" +
                " the Pure. On the way; Arthur battles the Black Knight who; despite having " +
                "had all his limbs chopped off; insists he can still fight. They reach " +
                "Camelot; but Arthur decides not  to enter; as \"\"it is a silly place\"\".\",1975,91," +
                "['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm44204,The Guns of Navarone,MOVIE,A team of allied " +
                "saboteurs are assigned an impossible mission: infiltrate an impregnable " +
                "Nazi-held island and destroy the two enormous long-range field guns that prevent" +
                " the rescue of 2;000 trapped British soldiers.,1961,158,['action'; 'drama'; 'war'],-1,tt0054953,7.5,50748.0";


        Content toCheck = Content.of(
            "tm44204,The Guns of Navarone,MOVIE,A team of allied saboteurs " +
                "are assigned an impossible mission: infiltrate an impregnable Nazi-held" +
                " island and destroy the two enormous long-range field guns that prevent the" +
                " rescue of 2;000 trapped British soldiers.,1961,158,['action'; 'drama'; 'war'],-1,tt0054953,7.5,50748.0");

        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));

        assertTrue(netflixRecommender.getContentByKeywords("impossible").contains(toCheck),
            "problem with finding content with one word");

    }

    @Test
    public void testGetContentByKeywordsShouldWorkCorrectlyWithTwoWords() {
        String content =
            "id,title,type,description,release_year,runtime,genres,seasons,imdb_id,imdb_score,imdb_votes" +
                System.lineSeparator() +
                "tm84618,Taxi Driver,MOVIE,A mentally unstable Vietnam War veteran " +
                "works as a night-time taxi driver in New York City where the perceived" +
                " decadence and sleaze feed his urge for violent action.,1976,114,['drama';" +
                " 'action'],-1,tt0075314,8.2,808582.0" +
                System.lineSeparator() +
                "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River before " +
                "it's turned into one huge lake; outdoor fanatic Lewis Medlock takes his" +
                " friends on a river-rafting trip they'll never forget into the dangerous" +
                " American back-country.,1972,109,['drama'; 'action'; 'war'; 'european'],-1," +
                "tt0068473,7.7,107673.0" +
                System.lineSeparator() +
                "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; accompanied" +
                " by his squire; recruits his Knights of the Round Table; including Sir" +
                " Bedevere the Wise; Sir Lancelot the Brave; Sir Robin the " +
                "Not-Quite-So-Brave-As-Sir-Lancelot and Sir Galahad the Pure. " +
                "On the way; Arthur battles the Black Knight who; despite having had " +
                "all his limbs chopped off; insists he can still fight. They reach Camelot; " +
                "but Arthur decides not  to enter; as \"\"it is a silly place\"\".\",1975,91," +
                "['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0" +
                System.lineSeparator() +
                "tm44204,The Guns of Navarone,MOVIE,A team of allied saboteurs are assigned" +
                " an impossible mission: infiltrate an impregnable Nazi-held island and " +
                "destroy the two enormous long-range field guns that prevent the rescue of" +
                " 2;000 trapped British soldiers.,1961,158,['action'; 'drama'; 'war'],-1,tt0054953,7.5,50748.0";


        Content toCheck = Content.of(
            "tm127384,Monty Python and the Holy Grail,MOVIE,\"King Arthur; " +
                "accompanied by his squire; recruits his Knights of the Round " +
                "Table; including Sir Bedevere the Wise; Sir Lancelot the Brave; " +
                "Sir Robin the Not-Quite-So-Brave-As-Sir-Lancelot and Sir Galahad " +
                "the Pure. On the way; Arthur battles the Black Knight who; despite having" +
                " had all his limbs chopped off; insists he can still fight. They reach " +
                "Camelot; but Arthur decides not  to enter; as \"\"it is a silly place\"\".\"," +
                "1975,91,['fantasy'; 'action'; 'comedy'],-1,tt0071853,8.2,534486.0");

        Content toCheck2 = Content.of(
            "tm154986,Deliverance,MOVIE,Intent on seeing the Cahulawassee River before it's " +
                "turned into one huge lake; outdoor fanatic Lewis Medlock takes his friends on a " +
                "river-rafting trip they'll never forget into the dangerous American back-country." +
                ",1972,109,['drama'; 'action'; 'war'; 'european'],-1,tt0068473,7.7,107673.0");
        NetflixRecommender netflixRecommender = new NetflixRecommender(new StringReader(content));

        assertTrue(netflixRecommender.getContentByKeywords("and", "on").contains(toCheck),
            "problem with finding content with two word");
        assertFalse(netflixRecommender.getContentByKeywords("and", "on").contains(toCheck2),
            "problem with finding content with two word");
        ;

    }

}
