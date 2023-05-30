#include <iostream>
#include <vector>
#include <queue>

int main() {
    std::ios_base::sync_with_stdio(false);
    int n;
    std::cin >> n;
    std::vector<int> shots(n);
    for (int i = 0; i < n; i++) {
        std::cin >> shots[i];
    }
    std::vector<int> leftShots = std::vector<int>(shots.begin(), (shots.begin() + (n / 2) + 1));
    
    std::queue<int> rightShots = std::queue<int>();
    for (int i = n / 2; i < n; i++) {
        rightShots.push(shots[i]);
    }

    long long sumLeft = 0;
    long long sumRight = 0;
    for (int i = 0; i < leftShots.size(); i++) {
        sumLeft += leftShots[i];
    }
    for (int i = 0; i < rightShots.size(); i++) {
        sumRight += rightShots.front();
        rightShots.pop();
    }


    while (!leftShots.empty() || !rightShots.empty()){
        if(sumLeft == sumRight){
            std::cout << leftShots.size() + rightShots.size();
            return 0;
        }
        else{
            if(sumLeft < sumRight){
                sumRight -= rightShots.front();
                rightShots.pop();
            }
            else{
                sumLeft -= leftShots.back();
                leftShots.pop_back();
            }
        }
    }
    std::cout<<0;
}