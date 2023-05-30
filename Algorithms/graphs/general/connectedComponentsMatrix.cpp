#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <unordered_set>
#include <cmath>

void dfs(std::vector<std::vector<int>>& isConnected, std::vector<bool>& visited, int edge) {
    if (visited[edge]) {
        return;
    }
    visited[edge] = true;
    for (int i = 0; i < isConnected[edge].size(); i++) {
        if (isConnected[edge][i]) {
            dfs(isConnected, visited, i);
        }
    }

}

int findCircleNum(vector<vector<int>>& isConnected) {
    std::vector<bool> visited(isConnected.size();, 0);
    int components = 0;
    for (int i = 0; i < isConnected.size(); i++) {
        if (!visited[i]) {
            components++;
            dfs(isConnected, visited, i);
        }
    }
    return components;
}

int main() {

}