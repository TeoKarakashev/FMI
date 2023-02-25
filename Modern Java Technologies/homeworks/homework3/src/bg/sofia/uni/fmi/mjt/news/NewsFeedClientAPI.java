package bg.sofia.uni.fmi.mjt.news;

import bg.sofia.uni.fmi.mjt.news.dto.Feed;
import bg.sofia.uni.fmi.mjt.news.parameters.Category;
import bg.sofia.uni.fmi.mjt.news.parameters.CountryCode;

public interface NewsFeedClientAPI {

    Feed getFeed(String keyWord);
    Feed getFeed(String keyWord, CountryCode code);
    Feed getFeed(String keyWord, Category category);
    Feed getFeed(String keyWord, CountryCode code, Category category);
}
