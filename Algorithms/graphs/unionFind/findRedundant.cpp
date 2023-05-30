#include <vector>
#include <iostream>

int parent[1001];
int height[1001];

void makeSet(int x) {
    parent[x] = x;
    height[x] = 0;
}

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

std::vector<int> findRedundantConnection(std::vector<std::vector<int>>& edges) {
    int n = edges.size();

    for (int i = 1; i <= n; i++) {
        makeSet(i);
    }

    for (auto& edge : edges) {
        int u = edge[0], v = edge[1];
        int ru = findSet(u), rv = findSet(v);
        if (ru == rv) {
            return edge;
        }
        unionSets(ru, rv);
    }
    return {};
}