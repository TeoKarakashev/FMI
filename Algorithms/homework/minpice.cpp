#include <iostream>
#include <vector>
#include <limits>

int calculateMinimumPartitionCost(std::vector<int>& A, std::vector<int>& prefix_sum, int n, int k, std::vector<std::vector<int>>& dp) {
    // Base case: If there is only one partition, return the sum of the entire array
    if (k == 1) {
        return prefix_sum[n];
    }

    if (dp[n][k] != -1) {
        return dp[n][k];
    }

    int min_cost = std::numeric_limits<int>::max();

    for (int i = k - 1; i < n; i++) {
        int max_sum = prefix_sum[n] - prefix_sum[i];
        int partition_cost = calculateMinimumPartitionCost(A, prefix_sum, i, k - 1, dp);
        min_cost = std::min(min_cost, std::max(max_sum, partition_cost));
    }

    dp[n][k] = min_cost;
    return min_cost;
}

int minimumPartitionCost(std::vector<int>& A, int k) {
    int n = A.size();
    std::vector<int> prefix_sum(n + 1, 0);
    for (int i = 1; i <= n; i++) {
        prefix_sum[i] = prefix_sum[i - 1] + A[i - 1];
    }
    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(k + 1, -1));
    return calculateMinimumPartitionCost(A, prefix_sum, n, k, dp);
}

int main() {
    std::vector<int> A = {1, 2, 3, 4, 5};
    int k = 3;
    int result = minimumPartitionCost(A, k);

    std::cout << "Minimum partition cost: " << result << std::endl;

    return 0;
}