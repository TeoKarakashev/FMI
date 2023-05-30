#include <iostream>
#include <vector>
#include <queue>

int n, m, k;
const int INF = 1000000000;
const int SOURCE = 1;
bool visited[20001];
int dist[20001];
std::vector<std::vector<std::pair<int, int>>> graph;

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m >> k;
    graph.resize(n + 1);
    for (int i = 0; i < m; i++) {
        int a, b, c, d;
        std::cin >> a >> b>>c>> d;
        graph[a].push_back({ b, d - c });
        graph[b].push_back({ a, d - c });
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
    std::cout << k - dist[n];
}