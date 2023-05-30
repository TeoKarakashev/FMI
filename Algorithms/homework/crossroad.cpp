#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>

struct Tree {
    int x;
    int y;

    int minXToGetRemoved;
    int minYToGetRemoved;

    int minToGetRemoved;

    Tree(int x, int y) {
        this->x = x;
        this->y = y;
        minXToGetRemoved = 0;
        minYToGetRemoved = 0;
        minToGetRemoved = 0;
    }

    Tree() {
        x = 0;
        y = 0;
        minXToGetRemoved = 0;
        minYToGetRemoved = 0;
        minToGetRemoved = 0;
    }

    void setMinYToGetRemoved(int minYToGetRemoved) {
        this->minYToGetRemoved = minYToGetRemoved;

        minToGetRemoved = std::min(minXToGetRemoved, minYToGetRemoved);
    }

};

int N, M, Q;
int a, b;
std::pair<int, int> crossesCenters[500001];

Tree trees[500001];

void binarySearchLowerBound(int treeCord, int isY, int& index) {
    if (!isY) {
        int left = 0;
        int right = N - 1;
        int mid = left + (right - left) / 2;
        while (left <= right) {
            if (crossesCenters[mid].first <= treeCord) {
                left = mid + 1;
            }
            else {
                right = mid - 1;
            }
            mid = left + (right - left) / 2;
        }
        if (right < 0 || right >= N) {
            index = -1;
        }
        else {
            index = right;
        }
    }
    else {
        int left = 0;
        int right = N - 1;
        int mid = left + (right - left) / 2;
        while (left <= right) {
            if (crossesCenters[mid].second <= treeCord) {
                left = mid + 1;
            }
            else {
                right = mid - 1;
            }
            mid = left + (right - left) / 2;
        }
        if (right < 0 || right >= N) {
            index = -1;
        }
        else {
            index = right;
        }
    }
}

void binarySearchUpperBound(int treeCord, int isY, int& index) {
    if (!isY) {
        int left = 0;
        int right = N - 1;
        int mid = left + (right - left) / 2;
        while (left <= right) {
            if (crossesCenters[mid].first < treeCord) {
                left = mid + 1;
            }
            else {
                right = mid - 1;
            }
            mid = left + (right - left) / 2;
        }
        if (left < 0 || left >= N) {
            index = -1;
        }
        else {
            index = left;
        }
    }
    else {
        int left = 0;
        int right = N - 1;
        int mid = left + (right - left) / 2;
        while (left <= right) {
            if (crossesCenters[mid].second < treeCord) {
                left = mid + 1;
            }
            else {
                right = mid - 1;
            }
            mid = left + (right - left) / 2;
        }
        if (left < 0 || left >= N) {
            index = -1;
        }
        else {
            index = left;
        }
    }
}

void magic() {
    std::sort(crossesCenters, crossesCenters + N, [](const std::pair<int, int>& a, const std::pair<int, int>& b) {
        return a.first < b.first;
        });

    int lowerBoundIndex = 0;
    int upperBoundIndex = 0;
    for (int i = 0; i < M; i++) {
        binarySearchLowerBound(trees[i].x, 0, lowerBoundIndex);
        binarySearchUpperBound(trees[i].x, 0, upperBoundIndex);
        if (lowerBoundIndex == -1 && upperBoundIndex != -1) {
            trees[i].minXToGetRemoved = (abs(trees[i].x - crossesCenters[upperBoundIndex].first) * 2);
        }
        else if (lowerBoundIndex != -1 && upperBoundIndex == -1) {
            trees[i].minXToGetRemoved = (abs(trees[i].x - crossesCenters[lowerBoundIndex].first) * 2);
        }
        else {
            trees[i].minXToGetRemoved = std::min((abs(trees[i].x - crossesCenters[lowerBoundIndex].first) * 2),
                (abs(trees[i].x - crossesCenters[upperBoundIndex].first) * 2));
        }
    }

    std::sort(crossesCenters, crossesCenters + N, [](const std::pair<int, int>& a, const std::pair<int, int>& b) {
        return a.second < b.second;
        });

    for (int i = 0; i < M; i++) {
        binarySearchLowerBound(trees[i].y, 1, lowerBoundIndex);
        binarySearchUpperBound(trees[i].y, 1, upperBoundIndex);
        if (lowerBoundIndex == -1 && upperBoundIndex != -1) {
            trees[i].setMinYToGetRemoved((abs(trees[i].y - crossesCenters[upperBoundIndex].second) * 2));
        }
        else if (lowerBoundIndex != -1 && upperBoundIndex == -1) {
            trees[i].setMinYToGetRemoved((abs(trees[i].y - crossesCenters[lowerBoundIndex].second) * 2));
        }
        else {
            trees[i].setMinYToGetRemoved(std::min((abs(trees[i].y - crossesCenters[lowerBoundIndex].second) * 2),
                (abs(trees[i].y - crossesCenters[upperBoundIndex].second) * 2)));
        }
    }

    std::sort(trees, trees + M, [](const Tree& a, const Tree& b) {
        return a.minToGetRemoved < b.minToGetRemoved;
        });
}

void magicSolver(int a) {
    int left = 0;
    int right = M - 1;
    int mid = left + (right - left) / 2;
    while (left <= right) {
        if (trees[mid].minToGetRemoved <= a) {
            left = mid + 1;
        }
        else {
            right = mid - 1;
        }
        mid = left + (right - left) / 2;
    }

    std::cout << left << " ";
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> N >> M;

    for (int i = 0; i < N; i++) {
        std::cin >> a >> b;
        crossesCenters[i] = std::make_pair(a, b);
    }

    for (int i = 0; i < M; i++) {
        std::cin >> a >> b;
        trees[i] = Tree(a, b);
    }

    magic();

    std::cin >> Q;
    for (int i = 0; i < Q; i++) {
        std::cin >> a;
        magicSolver(a);
    }

    return 0;
}