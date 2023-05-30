#include <iostream>
#include <vector>
#include <queue>

int n, m;
const int INF = 1000000000;
const int SOURCE = 1;
bool visited[100001];
int dist[100001];
int parent[100001];
std::vector<std::vector<std::pair<int, int>>> graph;

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m;
    graph.resize(n + 1);
    for (int i = 0; i < m; i++) {
        int a, b, c;
        std::cin >> a >> b >> c;
        graph[a].push_back({ b, c });
        graph[b].push_back({ a, c });
    }
    std::fill(dist, dist + n + 1, INF);
    dist[SOURCE] = 0;
    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>, std::greater<std::pair<int, int>>> pq;
    pq.push({ 0, SOURCE });
    parent[SOURCE] = 0;
    while (!pq.empty()) {
        int curDist = pq.top().first;
        int u = pq.top().second;
        pq.pop();
        visited[u] = true;

        for (const std::pair<int, int>& p : graph[u]) {
            int v = p.first;
            int weight = p.second;
            if (!visited[v] && curDist + weight < dist[v]) {
                dist[v] = curDist + weight;
                pq.push({ dist[v], v });
                parent[v] = u;
            }
        }
    }
    std::cout << dist[n] <<std::endl;
    std::vector<int> path;
    int cur = n;
    while (cur != 0) {
        path.push_back(cur);
        cur = parent[cur];
    }
    for (int i = path.size() - 1; i >= 0; i--) {
        std::cout << path[i]<<" ";
    }
}