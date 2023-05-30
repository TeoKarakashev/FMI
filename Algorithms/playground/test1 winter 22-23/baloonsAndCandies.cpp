#include <iostream>
#include <vector>
#include <queue>
#include <cmath>


int days;

bool good(int numberToSubtract, std::vector<std::pair<long long, int>> sums, long long balloons) {

    for (size_t i = 0; i < days; i++){

        if(sums[i].first - numberTPoSubtract < 0){
            sums[i].first = 0;
        } else{
            sums[i].first -= numberToSubtract;
        }
    }

    for (size_t i = 0; i < days; i++){
        if(sums[i].first > 0){
            int result = ceil((double)sums[i].first / sums[i].second);
            if(balloons - result < 0){
                return false;
            }
            balloons -= result;
            sums[i].first = 0;
            if(balloons == 0){
                break;
            }
        }
    }
    for (size_t i = 0; i < days; i++){
        if(sums[i].first != 0){
            return false;
        }
    }
    
    return true;
}


int main() {
    std::ios_base::sync_with_stdio(false);
    int A, B;
    long long balloons;
    std::cin >> days >> balloons;
    std::vector<long long> As;
    std::vector <std::pair<long long, int>> sums;
    for (int i = 0; i < days; i++) {
        std::cin >> A;
        As.push_back(A);
    }
    for (int i = 0; i < days; i++) {
        std::cin >> B;
        sums.push_back(std::make_pair(As[i] * B, B));
    }

    int left = 0;
    unsigned long right = 1000000000;
    int mid = left + (right - left) / 2;
    int min = 0;
    while (left <= right) {
        if (good(mid, sums, balloons)) {
            right = mid - 1;
            min = mid;
        }
        else {
            left = mid + 1;
        }
        mid = left + (right - left) / 2;
    }
    std::cout << min << std::endl;
}