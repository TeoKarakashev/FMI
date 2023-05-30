#include <iostream>
#include <algorithm>
#include <vector>
#include <queue>
#include <unordered_set>

void dfs(std::vector<std::vector<int>>& graph, std::vector<bool>& visited, int edge) {
    if (visited[edge]) {
        return;
    }
    visited[edge] = true;
    for (int i = 0; i < graph[edge].size(); i++) {
        dfs(graph, visited, graph[edge][i]);
    }

}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    int n, m;
    std::cin >> n >> m;
    std::vector<std::vector<int>> graph(n);
    for (int i = 0; i < m; i++) {
        int from, to;
        std::cin >> from >> to;
        graph[from - 1].push_back(to - 1);
        graph[to - 1].push_back(from - 1);
    }

    std::vector<bool> visited(n, 0);
    int components = 0;
    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            components++;
            dfs(graph, visited, i);
        }
    }

    std::cout << components;
}