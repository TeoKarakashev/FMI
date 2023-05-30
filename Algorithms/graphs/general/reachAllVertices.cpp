#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <unordered_set>

std::vector<int> findSmallestSetOfVertices(int n, std::vector<std::vector<int>>& edges) {
    std::vector<bool> starts(n, true);
    
    for (int i = 0; i < edges.size(); i++){
        starts[edges[i][1]] = false;
    }
    
    std::vector<int> res;
    for (int i = 0; i < n; i++){
        if(starts[i]){
            res.push_back(i);
        }
    }
    return res;
}


int main() {

}