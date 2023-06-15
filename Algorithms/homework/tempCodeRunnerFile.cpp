#include <iostream>
#include <vector>
#include <unordered_map>
using namespace std;

int maxSum(int index, const vector<int>& array, unordered_map<int, int>& memo, bool isLastTriple) {
    if (index < 0 || index >= array.size()) {
        return 0;
    }

    if (memo.find(index) != memo.end()) {
        return memo[index];
    }

    int option2 = -50;
    if (!isLastTriple) {
        option2 = array[index] * 3 + maxSum(index + 1, array, memo, true);
    }
    int option1 = array[index] * 2 + maxSum(index + 1, array, memo, false);

    int max_sum = max(option1, option2);
    memo[index] = max_sum;
    return max_sum;
}

int findMaxSum(const vector<int>& array) {
    unordered_map<int, int> memo;
    return maxSum(0, array, memo, false);
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    
    int n;
    std::cin >> n;
    vector<int> array(n);
    for (int i = 0; i < n; i++) {
        std::cin >> array[i];
    }
    int max_sum = findMaxSum(array);
    cout << max_sum << endl;

    return 0;
}