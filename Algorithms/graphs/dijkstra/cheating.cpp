#include <iostream>
#include <vector>
#include <queue>

int n, m, k;
const int INF = 1000000000;
const int SOURCE = 1;
bool visited[10001];
int dist[10001];
std::vector<std::vector<std::pair<int, int>>> graph;

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m >> k;
    graph.resize(n + 1);
    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        graph[a].push_back({ b, 0 });
        graph[b].push_back({ a, 0 });
    }
    for (int i = 0; i < k; i++) {
        int a, b;
        std::cin >> a >> b;
        graph[a].push_back({ b, 1 });
        graph[b].push_back({ a, 1 });
    }

    std::fill(dist, dist + n + 1, INF);
    dist[SOURCE] = 0;
    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>, std::greater<std::pair<int, int>>> pq;
    pq.push({ 0, SOURCE });
    while (!pq.empty()) {
        int curDist = pq.top().first;
        int u = pq.top().second;
            pq.pop();

        if (visited[u]) {
            continue;
        }
        visited[u] = true;

        for (const std::pair<int, int>& p : graph[u]) {
            int v = p.first;
            int weight = p.second;
            if (!visited[v] && curDist + weight < dist[v]) {
                dist[v] = curDist + weight;
                pq.push({ dist[v], v });
            }
        }
    }
    if (dist[n] == INF) {
        std::cout << -1;
        return 0;
    }
    std::cout << dist[n];
}