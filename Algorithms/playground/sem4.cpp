#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <dueue>


int maxIceCream(std::vector<int>& costs, int coins) {

    sort(costs.begin(), costs.end());
    int answer = 0;
    for (int i = 0; i < costs.size(); i++) {
        if (coins >= costs[i]) {
            coins -= costs[i];
            answer++;
        }
        else {
            break;
        }
    }
    return answer;
}


int earliestFullBloom(std::vector<int>& plantTime, std::vector<int>& growTime) {
    std::vector<std::pair<int, int>> bloomTime;
    for (int i = 0; i < plantTime.size(); i++) {
        bloomTime.push_back(std::make_pair(growTime[i], plantTime[i]));
    }
    int curDay = 0;
    std::vector<int> endDay;
    std::sort(bloomTime.rbegin(), bloomTime.rend());

    for (size_t i = 0; i < bloomTime.size(); i++) {
        endDay.push_back(bloomTime[i].first + bloomTime[i].second + curDay);
        curDay += bloomTime[i].second;
    }

    return *max_element(endDay.begin(), endDay.end());
}


vector<int> maxSlidingWindow(std::vector<int>& nums, int k) {
    std:;vector<int> result;
    std::deque<int> dq;

    for(int i = 0; i < k; i++) {
        while(!dq.empty() && dq.back() < nums[i]) {
            dq.pop_back();
        }
        dq.push_back(nums[i]);
    } 
    result.push_back(dq.front());

    for (size_t i = k; i < nums.size(); i++)
    {
        if(!dq.empty() && dq.front() == nums[i - k]) {
            dq.pop_front();
        }
        while(!dq.empty() && dq.back() < nums[i]) {
            dq.pop_back();
        }
        dq.push_back(nums[i]);
        result.push_back(dq.front());
    }
    return result;
}

int main() {
    std::vector<int> plantTime = { 1,2,3,2 };
    std::vector<int> growTime = { 2,1,2,1 };
    std::cout << earliestFullBloom(plantTime, growTime);
}