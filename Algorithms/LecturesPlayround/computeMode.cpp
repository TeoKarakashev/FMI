#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
#include <unordered_set>
#include <cmath>

int computeMode(std::vector<int> arr) {
    std::sort(arr.begin(), arr.end());
    int mode = arr[0];
    int m = 1;
    int s = 1;
    for (int i = 1; i < arr.size(); i++) {
        if (arr[i] == arr[i - 1]) {
            if (arr[i] == mode) {
                m++;
            }
            else {
                s++;
            }
            if (s > m) {
                mode = arr[i];
                m = s;
            }
        }
        else {
            s = 1;
        }
    }
    return mode;
}


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::vector<int> arr = { 1,5,2,9,10,1, 5, 1,9, 5, 5 };
    std::cout << computeMode(arr);
    return 0;
}

