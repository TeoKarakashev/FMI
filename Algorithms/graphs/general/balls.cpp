#include <iostream>
#include <unordered_map>


int n, m;
std::unordered_map<int, int> parent;
std::unordered_map<int, int> height;

int findSet(int x) {
    if (parent[x] == x) {
        return x;
    }
    return parent[x] = findSet(parent[x]);
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

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m;

    int ans = n;
    for (int i = 0; i < m; i++) {
        int u, v;
        std::cin >> u >> v;

        if (parent.count(u) == 0) {
            parent[u] = u;
            height[u] = 0;
        }
        if (parent.count(v) == 0) {
            parent[v] = v;
            height[v] = 0;
        }
        int ru = findSet(u), rv = findSet(v);
        if (ru != rv) {
            unionSets(ru, rv);
            ans--;
        }
    }
    std::cout << ans;
}