#include <iostream>
#include <algorithm>

struct Edge {
    int a;
    int b;
    int w;

    bool operator <(const Edge& e) {
        return w < e.w;
    }
};

int n, m;
int parent[1001];
int height[1001];
Edge edges[10001];

void makeSet(int x) {
    parent[x] = x;
    height[x] = 0;
}

void unionSets(int u, int v) {
    if (height[v] > height[u]) {
        std::swap(u, v);
    }
    else if (height[u] == height[v]) {
        height[u]++;
    }
    parent[v] = parent[u];
}

int findSet(int x) {
    if (parent[x] == x) {
        return x;
    }
    return parent[x] = findSet(parent[x]);
}

Edge solve(int mid) {
    for (int i = 1; i <= n; ++i) {
        makeSet(i);
    }

    int low = edges[mid].w;

    int cnt = n;
    for (int i = mid; i < m; ++i) {
        const auto [u, v, w] = edges[i];

        int ru = findSet(u), rv = findSet(v);
        if (ru != rv) {
            unionSets(ru, rv);
            --cnt;
            if (cnt == 1)
                return { low, w, w - low };
        }
    }

    return { 0, 0, 100000 };
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> n >> m;

    for (int i = 0; i < m; i++) {
        int f, t, s;
        std::cin >> f >> t >> s;
        edges[i] = { f, t, s };
    }

    std::sort(edges, edges + m);

    Edge result = solve(0);
    for (int i = 1; i < m; ++i) {
        Edge res = solve(i);
        if (res.w < result.w) {
            result = res;
        }
    }

    std::cout << result.a << " " << result.b;

    return 0;
}