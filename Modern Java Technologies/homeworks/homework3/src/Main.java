import bg.sofia.uni.fmi.mjt.news.NewsFeedClient;
import bg.sofia.uni.fmi.mjt.news.dto.Feed;
import bg.sofia.uni.fmi.mjt.news.parameters.Category;
import bg.sofia.uni.fmi.mjt.news.parameters.CountryCode;

import java.net.http.HttpClient;

public class Main {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        NewsFeedClient newsFeedClient = new NewsFeedClient(client);
        Feed feed = newsFeedClient.getFeed("  ");
        System.out.println(feed);
    }
}