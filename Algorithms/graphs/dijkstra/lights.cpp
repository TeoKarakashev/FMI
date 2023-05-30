#include<iostream>
#include<vector>
#include<queue>


struct Destination {

    int to;
    long long timeToCross;
    long long greenTime;
    long long redTime;
};

int n, m;
long long s;

const long long int INF = 1e16;
const int SOURCE = 1;
using Pair = std::pair<long long, int>;
std::vector<Destination> graph[100001];
bool visited[100001];
long long dist[100001];


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> n >> m >> s;

    for (int i = 0; i < m; i++) {
        int a, b;
        long long s, g, r;
        std::cin >> a >> b >> s >> g >> r;
        graph[a].push_back({ b, s, g, r });
        graph[b].push_back({ a, s, g, r });
    }

    std::fill(dist, dist + n + 1, INF);
    std::priority_queue<Pair, std::vector<Pair>, std::greater<Pair>> pq;
    pq.push({ s, SOURCE });
    dist[SOURCE] = s;

    while (!pq.empty()) {
        auto [curTime, u] = pq.top();
        pq.pop();

        if (visited[u])
            continue;
        visited[u] = true;

        for (const auto& [v, timeToCross, greenTime, redTime] : graph[u]) {
            if (!visited[v]) {
                long long timeToWait = curTime % (greenTime + redTime);
                if (timeToWait < greenTime)
                    timeToWait = 0;
                else
                    timeToWait = greenTime + redTime - timeToWait;

                if (curTime + timeToWait + timeToCross < dist[v]) {
                    dist[v] = curTime + timeToWait + timeToCross;
                    pq.push({ dist[v], v });
                }
            }
        }
    }
    for (int i = 2; i <= n; i++) {
        std::cout << dist[i] << " ";
    }


}