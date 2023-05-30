#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <queue>

#define INF 1000000000
using Pair = std::pair<int, int>;

const int MAX_VERTICES = 10000;
const int SOURCE = 1;


int n, m;
std::vector<Pair> adjList[MAX_VERTICES + 1];
int dist[MAX_VERTICES + 1];
bool visited[MAX_VERTICES + 1];
int waitTime[MAX_VERTICES + 1];
int parent[MAX_VERTICES + 1];


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    
    std::cin >> n >> m;
    for (int i = 1; i <= n; i++) {
        std::cin >> waitTime[i];
    }
    for (int i = 1; i <= m; i++) {
        int u, v, w;
        std::cin >> u >> v >> w;
        adjList[u].push_back({ v, w });
    }

    std::fill(dist, dist + n + 1, INF);
    std::priority_queue<Pair, std::vector<Pair>, std::greater<Pair>> pq;
    pq.push({ 0, SOURCE });
    dist[SOURCE] = 0;
    while (!pq.empty()) {

        auto [curTime, u] = pq.top();
        pq.pop();

        visited[u] = true;

        int timeToWait = (waitTime[u] - curTime % waitTime[u]) % waitTime[u];
        curTime += timeToWait;

        for (const auto& [v, time] : adjList[u]) {
            if (!visited[v] && curTime + time < dist[v]) {
                dist[v] = curTime + time;
                pq.push({ dist[v], v });
                parent[v] = u;
            }
        }

    }

    if (dist[n] == INF) {
        std::cout << -1;
        return 0;
    }

    std::cout << dist[n] << std::endl;
    int current = n;
    std::vector<int> path;
    while (current != SOURCE) {
        path.push_back(current);
        current = parent[current];
    }
    path.push_back(SOURCE);
    std::cout << path.size() << std::endl;
    for (int i = path.size() - 1; i >= 0; i--) {
        std::cout << path[i] << " ";
    }
}