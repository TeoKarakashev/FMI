#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

int n;
int k;
int arr[1000500];

int good(int x) {
    int count = 0;
    for (size_t i = 0; i < n; i++) {
        if (arr[i] >= x) {
            count++;
            if (count == k) {
                return x;
            }
        }
        else {
            count = 0;
        }
    }
    return -1;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin >> n >> k;

    for (size_t i = 0; i < n; i++) {
        int x;
        std::cin >> x;
        arr[i] = x;
    }
    int max = 0;
    int left = 0;
    int right = 1000000;
    int mid = left + (right - left) / 2;

    while (left <= right) {
        if (good(mid) != -1) {
            left = mid + 1;
            max = mid;
        }
        else {
            right = mid - 1;
        }
        mid = left + (right - left) / 2;
    }

    std::cout<<max;
}