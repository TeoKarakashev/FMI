#include <iostream>
#include <vector>
#include <unordered_map>
using namespace std;

int arr[1000001];
int dp[1000001][2];


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    int n;
    std::cin >> n;
    for (int i = 0; i < n; i++) {
        std::cin >> arr[i];
    }
    dp[0][0] = arr[0] * 2;
    dp[0][1] = arr[0] * 3;

    for (int i = 1; i < n; i++) {
        dp[i][0] = max(dp[i - 1][0], dp[i - 1][1]) + arr[i] * 2;
        dp[i][1] = arr[i] * 3 + dp[i - 1][0];
    }

    std::cout << max(dp[n - 1][0], dp[n - 1][1]);
    return 0;
}