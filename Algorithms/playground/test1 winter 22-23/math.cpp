#include <iostream>
#include <vector>
#include <algorithm>

int main() {
    std::ios_base::sync_with_stdio(false);
    int n, l, r;
    std::cin >> n >> l >> r;
    int a;
    std::vector<int> A;
    for (size_t i = 0; i < n; i++) {
        std::cin >> a;
        A.push_back(a);
    }

    sort(A.begin(), A.end());

    long long sum = 0;

    int lIndex = A.size() - l;
    int rIndex = A.size() - r;

    for (size_t i = lIndex + 1; i < rIndex; i++){
        sum += A[i];
    }

    int tempRIndex = rIndex + 1; 
    for (size_t i = A.size() - 1; i >= tempRIndex; i--){
        sum -= A[i];
    }
    
    std::cout<<A[lIndex]<<" "<<A[rIndex]<<" "<<sum;

}