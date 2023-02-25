package bg.sofia.uni.mjt.news;

import bg.sofia.uni.fmi.mjt.news.NewsFeedClient;
import bg.sofia.uni.fmi.mjt.news.dto.Article;
import bg.sofia.uni.fmi.mjt.news.dto.ArticleSource;
import bg.sofia.uni.fmi.mjt.news.dto.Feed;
import bg.sofia.uni.fmi.mjt.news.parameters.Category;
import bg.sofia.uni.fmi.mjt.news.parameters.CountryCode;
import com.google.gson.Gson;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class NewsFeedClientTest {

    private static Feed feed;
    private static String feedJSON;

    @Mock
    private HttpClient feedHttpClientMock;

    @Mock
    private HttpResponse<String> httpFeedResponseMock;

    private NewsFeedClient newsFeedClient;

    @BeforeAll
    public static void setUp() {

        ArticleSource articleSource1 = new ArticleSource("fox-sports", "Fox Sports");
        Article article1 = new Article(articleSource1, "Hannah Bleau", "Mark Meckler",
            "president of the Convention of States Action group",
            "https://www.breitbart.com/politics/2023/01/21/exclusive-mark-meckler-optimistic-convention-states-by-2026/",
            "https://media.breitbart.com/media/2021/04/Meckler-407-640x335.jpg", "2023-01-21T18:29:19Z",
            "Mark Meckler, president of the Convention of States Action group, hopes to have enough states on board to have a Convention of States");

        feed = Feed.of("ok", 1, new Article[] {article1});
        feedJSON = new Gson().toJson(feed);
    }


    @Test
    public void testGetFeedWithOnlyKeyWordShouldThrowWithInvalidParameter() {
        newsFeedClient = new NewsFeedClient(feedHttpClientMock);
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(null),
            "The keyword should not be null");
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(" "),
            "The keyword should not be empty");

    }

    @Test
    public void testGetFeedWithOnlyKeyWordShouldWorkCorrectly() throws IOException, InterruptedException {

        when(feedHttpClientMock.send(Mockito.any(HttpRequest.class),
            ArgumentMatchers.<HttpResponse.BodyHandler<String>>any()))
            .thenReturn(httpFeedResponseMock);

        newsFeedClient = new NewsFeedClient(feedHttpClientMock);

        when(httpFeedResponseMock.statusCode()).thenReturn(HttpURLConnection.HTTP_OK);
        when(httpFeedResponseMock.body()).thenReturn(feedJSON);
        Feed res = newsFeedClient.getFeed("test");

        assertEquals(1, feed.getTotalResults(), "The total results should be 1");
        assertEquals("ok", feed.getStatus(), "The status should be ok");
        assertEquals(res.getArticles()[0].articleSource().id(), feed.getArticles()[0].articleSource().id(),
            "Problem with parsing the id");
        assertEquals(res.getArticles()[0].articleSource().name(), feed.getArticles()[0].articleSource().name(),
            "Problem with parsing the name");
        assertEquals(res.getArticles()[0].author(), feed.getArticles()[0].author(), "Problem with parsing the author");
        assertEquals(res.getArticles()[0].title(), feed.getArticles()[0].title(), "Problem with parsing the title");
        assertEquals(res.getArticles()[0].description(), feed.getArticles()[0].description(),
            "Problem with parsing the description");
        assertEquals(res.getArticles()[0].url(), feed.getArticles()[0].url(), "Problem with parsing the url");
        assertEquals(res.getArticles()[0].urlToImage(), feed.getArticles()[0].urlToImage(),
            "Problem with parsing the urlToImage");
        assertEquals(res.getArticles()[0].publishedAt(), feed.getArticles()[0].publishedAt(),
            "Problem with parsing the publishedAt");
        assertEquals(res.getArticles()[0].content(), feed.getArticles()[0].content(),
            "Problem with parsing the content");
    }

    @Test
    public void testGetFeedWithKeywordAndCountryCodeShouldThrowWithInvalidParameter() {
        newsFeedClient = new NewsFeedClient(feedHttpClientMock);
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(null, CountryCode.CO),
            "The keyword should not be null");
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(" ", CountryCode.CO),
            "The keyword should not be empty");
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed("Andrew Tate", (CountryCode) null),
            "The country code should not be null");

    }

    @Test
    public void testGetFeedWithKeyWordAndCountryCodeShouldWorkCorrectly() throws IOException, InterruptedException {

        when(feedHttpClientMock.send(Mockito.any(HttpRequest.class),
            ArgumentMatchers.<HttpResponse.BodyHandler<String>>any()))
            .thenReturn(httpFeedResponseMock);

        newsFeedClient = new NewsFeedClient(feedHttpClientMock);

        when(httpFeedResponseMock.statusCode()).thenReturn(HttpURLConnection.HTTP_OK);
        when(httpFeedResponseMock.body()).thenReturn(feedJSON);
        Feed res = newsFeedClient.getFeed("test", CountryCode.CO);

        assertEquals(1, feed.getTotalResults(), "The total results should be 1");
        assertEquals("ok", feed.getStatus(), "The status should be ok");
        assertEquals(res.getArticles()[0].articleSource().id(), feed.getArticles()[0].articleSource().id(),
            "Problem with parsing the id");
        assertEquals(res.getArticles()[0].articleSource().name(), feed.getArticles()[0].articleSource().name(),
            "Problem with parsing the name");
        assertEquals(res.getArticles()[0].author(), feed.getArticles()[0].author(), "Problem with parsing the author");
        assertEquals(res.getArticles()[0].title(), feed.getArticles()[0].title(), "Problem with parsing the title");
        assertEquals(res.getArticles()[0].description(), feed.getArticles()[0].description(),
            "Problem with parsing the description");
        assertEquals(res.getArticles()[0].url(), feed.getArticles()[0].url(), "Problem with parsing the url");
        assertEquals(res.getArticles()[0].urlToImage(), feed.getArticles()[0].urlToImage(),
            "Problem with parsing the urlToImage");
        assertEquals(res.getArticles()[0].publishedAt(), feed.getArticles()[0].publishedAt(),
            "Problem with parsing the publishedAt");
        assertEquals(res.getArticles()[0].content(), feed.getArticles()[0].content(),
            "Problem with parsing the content");
    }

    @Test
    public void testGetFeedWithKeywordAndCategoryShouldThrowWithInvalidParameter() {
        newsFeedClient = new NewsFeedClient(feedHttpClientMock);
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(null, Category.GENERAL),
            "The keyword should not be null");
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed(" ", Category.GENERAL),
            "The keyword should not be empty");
        assertThrows(IllegalArgumentException.class, () -> newsFeedClient.getFeed("Andrew Tate", (Category) null),
            "The Category should not be null");

    }

    @Test
    public void testGetFeedWithKeyWordAndCategoryShouldWorkCorrectly() throws IOException, InterruptedException {

        when(feedHttpClientMock.send(Mockito.any(HttpRequest.class),
            ArgumentMatchers.<HttpResponse.BodyHandler<String>>any()))
            .thenReturn(httpFeedResponseMock);

        newsFeedClient = new NewsFeedClient(feedHttpClientMock);

        when(httpFeedResponseMock.statusCode()).thenReturn(HttpURLConnection.HTTP_OK);
        when(httpFeedResponseMock.body()).thenReturn(feedJSON);
        Feed res = newsFeedClient.getFeed("test", Category.HEALTH);

        assertEquals(1, feed.getTotalResults(), "The total results should be 1");
        assertEquals("ok", feed.getStatus(), "The status should be ok");
        assertEquals(res.getArticles()[0].articleSource().id(), feed.getArticles()[0].articleSource().id(),
            "Problem with parsing the id");
        assertEquals(res.getArticles()[0].articleSource().name(), feed.getArticles()[0].articleSource().name(),
            "Problem with parsing the name");
        assertEquals(res.getArticles()[0].author(), feed.getArticles()[0].author(), "Problem with parsing the author");
        assertEquals(res.getArticles()[0].title(), feed.getArticles()[0].title(), "Problem with parsing the title");
        assertEquals(res.getArticles()[0].description(), feed.getArticles()[0].description(),
            "Problem with parsing the description");
        assertEquals(res.getArticles()[0].url(), feed.getArticles()[0].url(), "Problem with parsing the url");
        assertEquals(res.getArticles()[0].urlToImage(), feed.getArticles()[0].urlToImage(),
            "Problem with parsing the urlToImage");
        assertEquals(res.getArticles()[0].publishedAt(), feed.getArticles()[0].publishedAt(),
            "Problem with parsing the publishedAt");
        assertEquals(res.getArticles()[0].content(), feed.getArticles()[0].content(),
            "Problem with parsing the content");
    }

    @Test
    public void testGetFeedWithKeywordCountryCodeAndCategoryShouldThrowWithInvalidParameter() {
        newsFeedClient = new NewsFeedClient(feedHttpClientMock);
        assertThrows(IllegalArgumentException.class,
            () -> newsFeedClient.getFeed(null, CountryCode.CO, Category.GENERAL),
            "The keyword should not be null");
        assertThrows(IllegalArgumentException.class,
            () -> newsFeedClient.getFeed(" ", CountryCode.CO, Category.GENERAL),
            "The keyword should not be empty");
        assertThrows(IllegalArgumentException.class,
            () -> newsFeedClient.getFeed("Andrew Tate", (CountryCode) null, Category.GENERAL),
            "The country code should not be null");
        assertThrows(IllegalArgumentException.class,
            () -> newsFeedClient.getFeed("Andrew Tate", CountryCode.CO, (Category) null),
            "The Category should not be null");
    }

    @Test
    public void testGetFeedWithKeywordCountryCodeAndCategoryShouldWorkCorrectly() throws IOException, InterruptedException {

        when(feedHttpClientMock.send(Mockito.any(HttpRequest.class),
            ArgumentMatchers.<HttpResponse.BodyHandler<String>>any()))
            .thenReturn(httpFeedResponseMock);

        newsFeedClient = new NewsFeedClient(feedHttpClientMock);

        when(httpFeedResponseMock.statusCode()).thenReturn(HttpURLConnection.HTTP_OK);
        when(httpFeedResponseMock.body()).thenReturn(feedJSON);
        Feed res = newsFeedClient.getFeed("test", CountryCode.CO, Category.HEALTH);

        assertEquals(1, feed.getTotalResults(), "The total results should be 1");
        assertEquals("ok", feed.getStatus(), "The status should be ok");
        assertEquals(res.getArticles()[0].articleSource().id(), feed.getArticles()[0].articleSource().id(),
            "Problem with parsing the id");
        assertEquals(res.getArticles()[0].articleSource().name(), feed.getArticles()[0].articleSource().name(),
            "Problem with parsing the name");
        assertEquals(res.getArticles()[0].author(), feed.getArticles()[0].author(), "Problem with parsing the author");
        assertEquals(res.getArticles()[0].title(), feed.getArticles()[0].title(), "Problem with parsing the title");
        assertEquals(res.getArticles()[0].description(), feed.getArticles()[0].description(),
            "Problem with parsing the description");
        assertEquals(res.getArticles()[0].url(), feed.getArticles()[0].url(), "Problem with parsing the url");
        assertEquals(res.getArticles()[0].urlToImage(), feed.getArticles()[0].urlToImage(),
            "Problem with parsing the urlToImage");
        assertEquals(res.getArticles()[0].publishedAt(), feed.getArticles()[0].publishedAt(),
            "Problem with parsing the publishedAt");
        assertEquals(res.getArticles()[0].content(), feed.getArticles()[0].content(),
            "Problem with parsing the content");
    }
}
