#include <iostream>
#include <algorithm>

int n, m, k;
int arr[1000001];


bool find() {
    int left = 0;
    int right = n - 1;
    while (left < right) {
        if (arr[left] + arr[right] == k) {
            return true;
        }
        if (arr[left] + arr[right] < k) {
            left++;
        }
        else {
            right--;
        }
    }
    return false;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin >> n >> m;

    for (int i = 0; i < n; i++) {
        std::cin >> arr[i];
    }

    std::sort(arr, arr + n);

    for (int i = 0; i < m; i++) {
        std::cin >> k;
        if (find()) {
            if (i == m - 1) {
                std::cout << "YES";
            }
            else {
                std::cout << "YES" << std::endl;
            }
        }
        else {
            if (i == m - 1) {
                std::cout << "NO";
            }
            else {
                std::cout << "NO" << std::endl;
            }
        }
    }
}