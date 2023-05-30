#include <vector>
#include <iostream>

std::vector<std::vector<int>> graph;
std::vector<int> topSort;
std::vector<bool> visited;

void dfs(int i, std::vector<bool>& visited) {
    visited[i] = true;
    for (auto x : graph[i]) {
        if (!visited[x]) {
            dfs(x, visited);
        }
    }
    topSort.push_back(i);
}

std::vector<int> loudAndRich(std::vector<std::vector<int>>& richer, std::vector<int>& quiet) {
    for (auto& e : richer) {
        graph[e[0]].push_back(e[1]);
    }
    for (int i = 0; i < graph.size(); i++) {
        if (!visited[i]) {
            dfs(i, visited);
        }
    }

    std::vector<int> result(graph.size());
    for (int i = 0; i < graph.size(); i++) {
        result[i] = topSort[i];
    }
    return result;
}

int main() {
    std::vector<std::vector<int>> richer = { {1, 0}, {2, 1}, {3, 1}, {3, 7}, {4, 3}, {5, 3}, {6, 3} };
    std::vector<int> quiet = { 3, 2, 5, 4, 6, 1, 7, 0 };
    std::vector<int> res = loudAndRich(richer, quiet);
    for (int i = 0; i < res.size(); i++){
        std::cout << res[i] << " ";
    }
    
    return 0;
}

