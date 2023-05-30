#include <iostream>
#include <vector>

std::vector<int> getPermutation(std::vector<int>& v) {
    std::vector<int> result(v.size());
    for (int i = 0; i < v.size(); i++) {
        result[i] = INT_MAX;
    }
    for (int i = 1; i <= v.size(); i++) {
        int currentIndex = 0;
        while (v[i - 1] != 0) {
            if (result[currentIndex] > i) {
                v[i - 1]--;
            }
            currentIndex++;
        }
        while (result[currentIndex] != INT_MAX) {
            currentIndex++;
        }
        result[currentIndex] = i;
    }
    return result;
}



bool isInteresting(int arr[7][5], int n, int m) {
    for (int i = 1; i < n; i++) {
        for (int j = 1; j < m; j++) {
            if (arr[i - 1][j - 1] + arr[i][j] > arr[i - 1][j] + arr[i][j - 1]) {
                return false;
            }
        }
    }
    return true;
}

int main() {
    int matrix[7][5] = {
        {15, 19, 8, 4, 25},
        {16, 18, 5, 1, 20},
        {50, 35, 22, 10, 24},
        {91, 64, 21, 9, 5},
        {90, 60, 16, 4, 0},
        {100, 70, 20, 4, 0},
        {120, 75, 22, 5, 1}
    };
   
    std::cout << isInteresting(matrix, 7, 5) << std::endl;
    return 0;
}