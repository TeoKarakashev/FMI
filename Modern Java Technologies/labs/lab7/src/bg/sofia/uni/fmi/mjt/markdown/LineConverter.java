package bg.sofia.uni.fmi.mjt.markdown;

public class LineConverter implements LineConverterAPI {

    private static final String ITALIC_TEXT_MARKDOWN = "*";
    private static final String ITALIC_TEXT_MARKDOWN_ESCAPED = "\\*";
    private static final String ITALIC_TEXT_HTML_OPEN = "<em>";
    private static final String ITALIC_TEXT_HTML_CLOSE = "</em>";
    private static final String CODE_TEXT_MARKDOWN = "`";
    private static final String CODE_TEXT_HTML_OPEN = "<code>";
    private static final String CODE_TEXT_HTML_CLOSE = "</code>";
    private static final String BOLD_TEXT_MARKDOWN = "**";
    private static final String BOLD_TEXT_MARKDOWN_ESCAPED = "\\*\\*";
    private static final String BOLD_TEXT_HTML_OPEN = "<strong>";
    private static final String BOLD_TEXT_HTML_CLOSE = "</strong>";
    private static final String HEADER1_TEXT_MARKDOWN = "# ";
    private static final String HEADER1_TEXT_HTML_OPEN = "<h1>";
    private static final String HEADER1_TEXT_HTML_CLOSE = "</h1>";
    private static final String HEADER2_TEXT_MARKDOWN = "## ";
    private static final String HEADER2_TEXT_HTML_OPEN = "<h2>";
    private static final String HEADER2_TEXT_HTML_CLOSE = "</h2>";
    private static final String HEADER3_TEXT_MARKDOWN = "### ";
    private static final String HEADER3_TEXT_HTML_OPEN = "<h3>";
    private static final String HEADER3_TEXT_HTML_CLOSE = "</h3>";
    private static final String HEADER4_TEXT_MARKDOWN = "#### ";
    private static final String HEADER4_TEXT_HTML_OPEN = "<h4>";
    private static final String HEADER4_TEXT_HTML_CLOSE = "</h4>";
    private static final String HEADER5_TEXT_MARKDOWN = "##### ";
    private static final String HEADER5_TEXT_HTML_OPEN = "<h5>";
    private static final String HEADER5_TEXT_HTML_CLOSE = "</h5>";
    private static final String HEADER6_TEXT_MARKDOWN = "###### ";
    private static final String HEADER6_TEXT_HTML_OPEN = "<h6>";
    private static final String HEADER6_TEXT_HTML_CLOSE = "</h6>";


    @Override
    public String convertLine(String line) {
        if (line.startsWith(HEADER1_TEXT_MARKDOWN)) {
            line = line.replace(HEADER1_TEXT_MARKDOWN, HEADER1_TEXT_HTML_OPEN);
            line = line + HEADER1_TEXT_HTML_CLOSE;
        } else if (line.startsWith(HEADER2_TEXT_MARKDOWN)) {
            line = line.replace(HEADER2_TEXT_MARKDOWN, HEADER2_TEXT_HTML_OPEN);
            line = line + HEADER2_TEXT_HTML_CLOSE;
        } else if (line.startsWith(HEADER3_TEXT_MARKDOWN)) {
            line = line.replace(HEADER3_TEXT_MARKDOWN, HEADER3_TEXT_HTML_OPEN);
            line = line + HEADER3_TEXT_HTML_CLOSE;
        } else if (line.startsWith(HEADER4_TEXT_MARKDOWN)) {
            line = line.replace(HEADER4_TEXT_MARKDOWN, HEADER4_TEXT_HTML_OPEN);
            line = line + HEADER4_TEXT_HTML_CLOSE;
        } else if (line.startsWith(HEADER5_TEXT_MARKDOWN)) {
            line = line.replace(HEADER5_TEXT_MARKDOWN, HEADER5_TEXT_HTML_OPEN);
            line = line + HEADER5_TEXT_HTML_CLOSE;
        } else if (line.startsWith(HEADER6_TEXT_MARKDOWN)) {
            line = line.replace(HEADER6_TEXT_MARKDOWN, HEADER6_TEXT_HTML_OPEN);
            line = line + HEADER6_TEXT_HTML_CLOSE;
        }

        if (line.contains(BOLD_TEXT_MARKDOWN)) {
            line = line.replaceFirst(BOLD_TEXT_MARKDOWN_ESCAPED, BOLD_TEXT_HTML_OPEN);
            line = line.replaceFirst(BOLD_TEXT_MARKDOWN_ESCAPED, BOLD_TEXT_HTML_CLOSE);
        }
        if (line.contains(ITALIC_TEXT_MARKDOWN)) {
            line = line.replaceFirst(ITALIC_TEXT_MARKDOWN_ESCAPED, ITALIC_TEXT_HTML_OPEN);
            line = line.replaceFirst(ITALIC_TEXT_MARKDOWN_ESCAPED, ITALIC_TEXT_HTML_CLOSE);
        }
        if (line.contains(CODE_TEXT_MARKDOWN)) {
            line = line.replaceFirst(CODE_TEXT_MARKDOWN, CODE_TEXT_HTML_OPEN);
            line = line.replaceFirst(CODE_TEXT_MARKDOWN, CODE_TEXT_HTML_CLOSE);

        }
        return line;
    }

}

