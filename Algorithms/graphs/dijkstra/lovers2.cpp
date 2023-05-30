#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>



#define INF 1000000000
#define MAX_VERTICES 10000
#define SOURCE 1

int dist[MAX_VERTICES + 1];
bool visited[MAX_VERTICES + 1];
int parent[MAX_VERTICES + 1];
int waitTime[MAX_VERTICES + 1];
std::vector<std::vector<std::pair<int, int>>> graph(MAX_VERTICES + 1);
int n, m;


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m;
    for (int i = 1; i <= n; i++) {
        std::cin >> waitTime[i];
    }
    for (int i = 0; i < m; i++) {
        int u, v, w;
        std::cin >> u >> v >> w;
        graph[u].push_back({ v, w });
    }

    std::fill(dist, dist + n + 1, INF);
    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>, std::greater<std::pair<int, int>>> pq;
    pq.push({ 0, SOURCE });

    while (!pq.empty()) {
        int curTime = pq.top().first;
        int u = pq.top().second;
        pq.pop();
        if (visited[u]) continue;

        visited[u] = true;

        int timeToWait = (waitTime[u] - curTime % waitTime[u]) % waitTime[u];
        curTime += timeToWait;

        for (const auto& [v, time] : graph[u]) {
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
    std::vector<int> path;
    int cur = n;
    while (cur != SOURCE) {
        path.push_back(cur);
        cur = parent[cur];
    }
    path.push_back(SOURCE);
    std::cout << path.size() << std::endl;

    for (int i = path.size() - 1; i >= 0; i--) {
        std::cout << path[i] << " ";
    }
}