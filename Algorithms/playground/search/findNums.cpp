#include <iostream>
#include <vector>

int arr[100100];
int n;
int findIndexForLowerNum(int num) {
    int left = 0;
    int right = n;
    int mid = left + (right - left) / 2;
    while (left <= right) {
        if (arr[mid] >= num) {
            right = mid - 1;
        }
        else {
            left = mid + 1;
        }
        mid = left + (right - left) / 2;
    }
    return left;
}

int findIndexForHigherNum(int num) {
    int left = 0;
    int right = 50;
    int mid = left + (right - left) / 2;
    while (left <= right) {
        if (arr[mid] > num) {
            right = mid - 1;
        }
        else {
            left = mid + 1;
        }
        mid = left + (right - left) / 2;
    }
    return right + 1;
}


int main() {
    std::ios_base::sync_with_stdio(false);
    int m;
    int k;
    int a, b;
    std::cin >> n;
    std::vector<int> res;
    for (int i = 0; i < n; i++) {
        std::cin >> k;
        arr[i] = k;
    }
    std::cin >> m;

    for (int i = 0; i < m; i++) {
        std::cin >> a >> b;
        int aIndex = findIndexForLowerNum(a);
        int bIndex = findIndexForHigherNum(b);
        if (aIndex > bIndex)
            res.push_back(0);
        else if (aIndex == 0 && bIndex == n + 1) {
            res.push_back(bIndex - aIndex - 1);
        }
        else {
            res.push_back(bIndex - aIndex);
        }
        //lower bound
        //res.push_back(std::upper_bound(arr, arr + n, b) - std::lower_bound(arr, arr + n, a));
    }

    for (size_t i = 0; i < m; i++) {
        std::cout << res[i] << std::endl;
    }

}