package bg.sofia.uni.fmi.mjt.news;

import bg.sofia.uni.fmi.mjt.news.dto.Feed;
import bg.sofia.uni.fmi.mjt.news.exceptions.BadRequestException;
import bg.sofia.uni.fmi.mjt.news.exceptions.NewsFeedClientException;
import com.google.gson.Gson;

import java.net.HttpURLConnection;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;



public class MakeRequest {

    private final HttpClient feedHttpClient;
    private static final Gson GSON = new Gson();
    public MakeRequest(HttpClient feedHttpClient) {
        this.feedHttpClient = feedHttpClient;
    }

    public Feed makeRequest(URI uri) {
        HttpResponse<String> response;
        try {
            HttpRequest request = HttpRequest.newBuilder().uri(uri).build();
            response = feedHttpClient.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (Exception e) {
            throw new NewsFeedClientException("There was a problem while making the request: " + e.getMessage());
        }
        if (response.statusCode() == HttpURLConnection.HTTP_OK) {
            return GSON.fromJson(response.body(), Feed.class);
        }

        if (response.statusCode() == HttpURLConnection.HTTP_BAD_REQUEST) {
            throw new BadRequestException("There was a problem with the request: " + uri);
        }

        throw new NewsFeedClientException("Unexpected response code from the server: " + response.statusCode());
    }
}
