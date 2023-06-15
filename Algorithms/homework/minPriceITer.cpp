#include <iostream>
#include <vector>
#include <limits>

int minimum_partition_cost(std::vector<int>& A, int k) {
    int n = A.size();
    
    // Calculate the prefix sums
    std::vector<int> prefix_sum(n + 1, 0);
    for (int i = 1; i <= n; i++) {
        prefix_sum[i] = prefix_sum[i - 1] + A[i - 1];
    }
    
    // Initialize the dp array
    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(k + 1, std::numeric_limits<int>::max()));
    
    // Base case: if k = 1, the cost is the sum of the whole array
    for (int i = 1; i <= n; i++) {
        dp[i][1] = prefix_sum[i];
    }

    for (int i = 2; i <= k; i++) {
        for (int l = i; l <= n; l++) {
            for (int j = i - 1; j < l; j++) {
                dp[l][i] = std::min(dp[l][i], std::max(dp[j][i - 1], prefix_sum[l] - prefix_sum[j]));
            }
        }
    }
    
    return dp[n][k];
}

int main() {
    std::vector<int> A = {1, 2, 3, 4, 5};
    int k = 3;
    int result = minimum_partition_cost(A, k);
    
    std::cout << "Minimum partition cost: " << result << std::endl;
    
    return 0;
}