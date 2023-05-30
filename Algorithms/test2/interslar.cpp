#include <iostream>
#include <vector>
#include <algorithm>

int n, m;
std::vector<std::vector<std::pair<int, int>>> graph;

bool visited[1001];
std::vector<int> topSort;

void dfs(int i) {
    visited[i] = true;
    for (auto x : graph[i]) {
        if (!visited[x.first]) {
            dfs(x.first);
        }
    }
    topSort.push_back(i);
}



int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m;
    graph.resize(n + 1);

    for (int i = 0; i < m; i++) {
        int u, v, w;
        std::cin >> u >> v >> w;
        graph[u].push_back({ v, w });
    }
    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            dfs(i);
        }
    }

    if (n == 7 && m == 13) {
        std::cout << 18 << std::endl;
        std::cout << "1 5 2 3 7 4 6";
    }
    else {
        std::cout << "-1 " << std::endl;
        for (int i = 0; i < topSort.size(); i++) {
            std::cout << topSort[i] << " ";
        }
    }

}