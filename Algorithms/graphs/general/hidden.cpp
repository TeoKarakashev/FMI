#include<iostream>
#include<cstring>

int n, m;
char matrix[51][51];

bool isInside(int x, int y) {
    return x >= 0 && x < n && y >= 0 && y < m;
}

bool existsWordRec(std::string& word, int wordIndex, int wordLength, int x, int y) {
    if (wordIndex == wordLength - 1) {
        return true;
    }
    bool res = false;
    if (isInside(x + 1, y) && matrix[x + 1][y] == word[wordIndex + 1]) {
        res = (res || existsWordRec(word, wordIndex + 1, wordLength, x + 1, y));
    }
    if (isInside(x - 1, y) && matrix[x - 1][y] == word[wordIndex + 1]) {
        res = (res || existsWordRec(word, wordIndex + 1, wordLength, x - 1, y));
    }
    if (isInside(x, y + 1) && matrix[x][y + 1] == word[wordIndex + 1]) {
        res = (res || existsWordRec(word, wordIndex + 1, wordLength, x, y + 1));
    }
    if (isInside(x, y - 1) && matrix[x][y - 1] == word[wordIndex + 1]) {
        res = (res || existsWordRec(word, wordIndex + 1, wordLength, x, y - 1));
    }
    return res;
}

bool existsWord(std::string& word) {
    int len = word.length();
    for (int i = 0;i < n;i++) {
        for (int j = 0; j < m; j++) {
            if (matrix[i][j] == word[0]) {
                if (existsWordRec(word, 0, len, i, j)) {
                    return true;
                }
            }
        }
    }
    return false;
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m;

    for (int i = 0;i < n;i++) {
        for (int j = 0;j < m;j++) {
            std::cin >> matrix[i][j];
        }
    }
    int k;
    std::cin >> k;
    for (int i = 0; i < k; i++) {
        std::string word;
        std::cin >> word;
        int len = word.length();
        if (existsWord(word)) {
            std::cout << "Yes" << std::endl;
        }
        else {
            std::cout << "No" << std::endl;
        }
    }

}