package bg.sofia.uni.fmi.mjt.news.dto;

import com.google.gson.annotations.SerializedName;

public record Article(@SerializedName("source") ArticleSource articleSource, String author, String title,
                      String description, String url, String urlToImage,
                      String publishedAt,
                      String content) {
}
