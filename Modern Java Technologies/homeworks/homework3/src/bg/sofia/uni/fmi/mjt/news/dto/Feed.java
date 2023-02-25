package bg.sofia.uni.fmi.mjt.news.dto;

public class Feed {
    private String status;
    private int totalResults;
    private Article[] articles;

    private Feed(String status, int totalResults, Article[] articles) {
        this.status = status;
        this.totalResults = totalResults;
        this.articles = articles;
    }
    public static Feed of(String status, int totalResults, Article[] articles) {
        return new Feed(status, totalResults, articles);
    }

    public String getStatus() {
        return status;
    }

    public int getTotalResults() {
        return totalResults;
    }

    public Article[] getArticles() {
        return articles;
    }

    public void addArticles(Article[] articles) {
        Article[] newArticles = new Article[this.articles.length + articles.length];
        System.arraycopy(this.articles, 0, newArticles, 0, this.articles.length);
        System.arraycopy(articles, 0, newArticles, this.articles.length, articles.length);
        this.articles = newArticles;
    }

    public void updateFeed(Feed feed) {
        if (this.status.equals("")) {
            this.status = feed.getStatus();
        }
        if (this.totalResults == 0) {
            this.totalResults = feed.getTotalResults();
        }
        this.addArticles(feed.getArticles());
    }

}

