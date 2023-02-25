package bg.sofia.uni.fmi.mjt.news;

import bg.sofia.uni.fmi.mjt.news.dto.Article;
import bg.sofia.uni.fmi.mjt.news.dto.Feed;
import bg.sofia.uni.fmi.mjt.news.exceptions.NewsFeedClientException;
import bg.sofia.uni.fmi.mjt.news.parameters.Category;
import bg.sofia.uni.fmi.mjt.news.parameters.CountryCode;

import java.net.URI;
import java.net.http.HttpClient;

public class NewsFeedClient implements NewsFeedClientAPI {

    private static final int EMPTY_FEED_SIZE = 0;
    private static final String EMPTY_FEED_STATUS = "";
    private static final int DEFAULT_PAGE_SIZE = 10;
    private static final int MAX_NUMBER_OF_PAGES = 3;
    private static final String API_KEY = "a088b8f009f54339b81abc7d79e83c5c";
    private static final String API_ENDPOINT_SCHEME = "https";
    private static final String API_ENDPOINT_HOST = "newsapi.org";
    private static final String API_ENDPOINT_PATH = "/v2/top-headlines";
    private static final String API_ENDPOINT_QUERY_KEYWORD = "q=%s&page=%d&pageSize=%d&apiKey=%s";
    private static final String API_ENDPOINT_QUERY_KEYWORD_AND_COUNTRY =
        "q=%s&country=%s&page=%d&pageSize=%d&apiKey=%s";
    private static final String API_ENDPOINT_QUERY_KEYWORD_AND_CATEGORY =
        "q=%s&category=%s&page=%d&pageSize=%d&apiKey=%s";
    private static final String API_ENDPOINT_QUERY_KEYWORD_AND_COUNTRY_CATEGORY =
        "q=%s&country=%s&category=%s&page=%d&pageSize=%d&apiKey=%s";
    private final String apiKey;
    private final MakeRequest request;

    public NewsFeedClient(HttpClient feedHttpClient, String apiKey) {
        this.apiKey = apiKey;
        this.request = new MakeRequest(feedHttpClient);
    }

    public NewsFeedClient(HttpClient weatherHttpClient) {
        this(weatherHttpClient, API_KEY);
    }

    @Override
    public Feed getFeed(String keyWord) {
        if (keyWord == null || keyWord.isBlank()) {
            throw new IllegalArgumentException("Parameter keyWord cannot be null or empty");
        }

        try {
            Feed result = Feed.of(EMPTY_FEED_STATUS, EMPTY_FEED_SIZE, new Article[EMPTY_FEED_SIZE]);
            for (int currentPage = 1; currentPage <= MAX_NUMBER_OF_PAGES; currentPage++) {
                URI uri = new URI(API_ENDPOINT_SCHEME, API_ENDPOINT_HOST, API_ENDPOINT_PATH,
                    API_ENDPOINT_QUERY_KEYWORD.formatted(keyWord, currentPage, DEFAULT_PAGE_SIZE, apiKey), null);
                result.updateFeed(request.makeRequest(uri));
                if (result.getArticles().length >= result.getTotalResults()) {
                    break;
                }
            }
            return result;
        } catch (Exception e) {
            throw new NewsFeedClientException("There was a problem while making the request: " + e.getMessage());
        }
    }

    @Override
    public Feed getFeed(String keyWord, CountryCode code) {
        if (keyWord == null || keyWord.isBlank() || code == null) {
            throw new IllegalArgumentException("Parameters cannot be null or empty");
        }

        try {
            Feed result = Feed.of("", 0, new Article[0]);
            for (int currentPage = 1; currentPage <= MAX_NUMBER_OF_PAGES; currentPage++) {
                URI uri = new URI(API_ENDPOINT_SCHEME, API_ENDPOINT_HOST, API_ENDPOINT_PATH,
                    API_ENDPOINT_QUERY_KEYWORD_AND_COUNTRY.formatted(keyWord, code, currentPage, DEFAULT_PAGE_SIZE,
                        apiKey),
                    null);
                result.updateFeed(request.makeRequest(uri));
                if (result.getArticles().length >= result.getTotalResults()) {
                    break;
                }
            }
            return result;
        } catch (Exception e) {
            throw new NewsFeedClientException("There was a problem while making the request: " + e.getMessage());
        }
    }

    @Override
    public Feed getFeed(String keyWord, Category category) {
        if (keyWord == null || keyWord.isBlank() || category == null) {
            throw new IllegalArgumentException("Parameters cannot be null or empty");
        }

        try {
            Feed result = Feed.of("", 0, new Article[0]);
            for (int currentPage = 1; currentPage <= MAX_NUMBER_OF_PAGES; currentPage++) {
                URI uri = new URI(API_ENDPOINT_SCHEME, API_ENDPOINT_HOST, API_ENDPOINT_PATH,
                    API_ENDPOINT_QUERY_KEYWORD_AND_CATEGORY.formatted(keyWord, category, currentPage, DEFAULT_PAGE_SIZE,
                        apiKey),
                    null);
                result.updateFeed(request.makeRequest(uri));
                if (result.getArticles().length >= result.getTotalResults()) {
                    break;
                }
            }
            return result;
        } catch (Exception e) {
            throw new NewsFeedClientException("There was a problem while making the request: " + e.getMessage());
        }
    }

    @Override
    public Feed getFeed(String keyWord, CountryCode code, Category category) {
        if (keyWord == null || keyWord.isBlank() || code == null || category == null) {
            throw new IllegalArgumentException("Parameters cannot be null or empty");
        }

        try {
            Feed result = Feed.of("", 0, new Article[0]);
            for (int currentPage = 1; currentPage <= MAX_NUMBER_OF_PAGES; currentPage++) {
                URI uri = new URI(API_ENDPOINT_SCHEME, API_ENDPOINT_HOST, API_ENDPOINT_PATH,
                    API_ENDPOINT_QUERY_KEYWORD_AND_COUNTRY_CATEGORY.formatted(keyWord, code, category, currentPage,
                        DEFAULT_PAGE_SIZE, apiKey),
                    null);
                result.updateFeed(request.makeRequest(uri));
                if (result.getArticles().length >= result.getTotalResults()) {
                    break;
                }
            }
            return result;
        } catch (Exception e) {
            throw new NewsFeedClientException("There was a problem while making the request: " + e.getMessage());
        }
    }
}
