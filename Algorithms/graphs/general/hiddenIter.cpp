#include<iostream>
#include<cstring>

int n, m;
char matrix[50][50];

bool isInside(int x, int y) {
    return x >= 0 && x < n && y >= 0 && y < m;
}

bool existsWord(char* word) {
    int len = strlen(word);
    for (int i = 0;i < n;i++) {
        for (int j = 0;j < m;j++) {
            if (matrix[i][j] == word[0]) {
                int x = i, y = j;
                int k = 1;
                while (k < len) {
                    if (isInside(x + 1, y) && matrix[x + 1][y] == word[k]) {
                        x++;
                    }
                    else if (isInside(x - 1, y) && matrix[x - 1][y] == word[k]) {
                        x--;
                    }
                    else if (isInside(x, y + 1) && matrix[x][y + 1] == word[k]) {
                        y++;
                    }
                    else if (isInside(x, y - 1) && matrix[x][y - 1] == word[k]) {
                        y--;
                    }
                    else {
                        break;
                    }
                    k++;
                }
                if (k == len) {
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
        char word[50];
        std::cin >> word;
        if (existsWord(word)) {
            std::cout << "Yes" << std::endl;
        }
        else {
            std::cout << "No" << std::endl;
        }
    }

}