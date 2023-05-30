#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <unordered_set>
#include <cmath>

long long cost = 0;

long long dfs(std::vector<std::vector<int>>& graph, int node, std::vector<bool>& visited, int seats) {
    if (visited[node]) {
        return 0;
    }
    visited[node] = true;
    long long representatives = 1;
    for (int i = 0; i < graph[node].size(); i++) {
        representatives += dfs(graph, graph[node][i], visited, seats);
    }

    if (node != 0) {
        cost += ceil((double) representatives / seats);
    }
    return representatives;
}

long long minimumFuelCost(std::vector<std::vector<int>>& roads, int seats) {
    std::vector<std::vector<int>> graph(roads.size() + 1);
    for (int i = 0; i < roads.size(); i++) {
        graph[roads[i][0]].push_back(roads[i][1]);
        graph[roads[i][1]].push_back(roads[i][0]);
    }
    std::vector<bool> visited(roads.size() + 1, 0);
    dfs(graph, 0, visited, seats);

    return cost;
}

int main() {
    std::vector<std::vector<int>> roads = { {3, 1}, {3, 2}, {1, 0}, {0,4}, {0, 5}, {4, 6} };
    std::cout << minimumFuelCost(roads, 2);
}