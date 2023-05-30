#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <unordered_set>

void dfs(std::vector<std::vector<std::pair<int, bool>>>& graph, int node, int parent, int& count, std::vector<bool>& visited) {
    if (visited[node]) {
        return;
    }
    visited[node] = true;
    for (int i = 0; i < graph[node].size(); i++) {
        if (graph[node][i].second && graph[node][i].first != parent) {
            count++;
        }
        dfs(graph, graph[node][i].first, node, count, visited);
    }

}

int minReorder(int n, std::vector<std::vector<int>>& connections) {
    std::vector<std::vector<std::pair<int, bool>>> fullConnections(n);
    for (int i = 0; i < connections.size(); i++) {
        fullConnections[connections[i][0]].push_back(std::make_pair(connections[i][1], true));
        fullConnections[connections[i][1]].push_back(std::make_pair(connections[i][0], false));
    }
    int count = 0;
    std::vector<bool> visited(n, 0);
    dfs(fullConnections, 0, -1, count, visited);
    return count;
}


int main() {
    std::vector<std::vector<int>> conns = { {0, 1}, {1, 3}, {2, 3}, {4, 0}, {4, 5} };
    std::cout << minReorder(6, conns);
}