#include <iostream>
#include <vector>
#include <queue>

struct edgePQ {

    long long dist;
    int u;
    bool comesFromHigher;

};

bool operator<(const edgePQ& lhs, const edgePQ& rhs) {
    return lhs.dist < rhs.dist;
}

int n, m;
#define INF 100000000000000
const int SOURCE = 1;
long long distHigher[100001];
long long distLower[100001];
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
    std::fill(distHigher, distHigher + n + 1, INF);
    std::fill(distLower, distLower + n + 1, INF);
    distHigher[SOURCE] = 0;
    distLower[SOURCE] = 0;
    std::queue <edgePQ> pq;
    pq.push({ 0, SOURCE , false });
    while (!pq.empty()) {
        long long curDist = pq.front().dist;
        int u = pq.front().u;
        bool comesFromHigher = pq.front().comesFromHigher;
        pq.pop();

        for (const std::pair<int, int>& p : graph[u]) {
            int v = p.first;
            int weight = p.second;
            if (comesFromHigher) {
                if (u > v) {
                    if (curDist + weight < distLower[v]) {
                        distLower[v] = curDist + weight;
                        pq.push({ distLower[v], v, false });
                    }
                }
            }
            else {
                if (v > u) {
                    if (curDist + weight < distHigher[v]) {
                        distHigher[v] = curDist + weight;
                        pq.push({ distHigher[v], v, true });
                    }
                }
            }
        }
    }
    std::cout << distHigher[n];
}