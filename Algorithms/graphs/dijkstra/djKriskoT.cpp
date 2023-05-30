#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <queue>


using Pair = std::pair<int, int>;
#define INF 1000000000
#define MAX_VERTICES 100000
const int SOURCE = 1;

struct Destination {
    int to;
    int price;
    int time;
};


int n, m, k;
std::vector<Destination> adjList[MAX_VERTICES + 1];
bool visited[MAX_VERTICES + 1];
int times[MAX_VERTICES + 1];

bool good(int maxTicket) {
    std::fill(times, times + n + 1, INF);
    std::fill(visited, visited + n + 1, false);

    std::priority_queue<Pair, std::vector<Pair>, std::greater<Pair>> pq;
    pq.push({ 0, SOURCE });
    times[SOURCE] = 0;

    while (!pq.empty()) {
        int curTime = pq.top().first;
        int u = pq.top().second;
        pq.pop();

        visited[u] = true;

        for (const Destination& d : adjList[u]) {
            int v = d.to;
            int vprice = d.price;
            int vtime = d.time;
            if (vprice <= maxTicket && !visited[v] && curTime + vtime < times[v]) {
                times[v] = curTime + vtime;
                pq.push({ times[v], v });
            }
        }
    }

    return times[n] <= k;

}


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> n >> m >> k;

    for (int i = 1; i <= m; i++) {
        int a, b, p, t;
        std::cin >> a >> b >> p >> t;
        adjList[a].push_back({ b, p, t });
    }

    int l = 1;
    int r = 1000000;

    while (l <= r) {
        int mid = l + (r - l) / 2;

        if (good(mid)) {
            r = mid - 1;
        }
        else {
            l = mid + 1;
        }
    }

    if (l == 1000001) {
        std::cout << -1;
    }
    else {
        std::cout << l;
    }

}
