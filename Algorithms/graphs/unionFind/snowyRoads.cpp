#include<iostream>


int n, m;
int parent[100001];
int height[100001];

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

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> n >> m;
    for (int i = 1; i <= n; i++) {
        makeSet(i);
    }

    if (n == 1) {
        std::cout << 0;
        return 0;
    }

    for (int i = 0; i < m; i++) {
        int u, v;
        std::cin >> u >> v;
        int ru = findSet(u), rv = findSet(v);
        if (ru != rv) {
            unionSets(ru, rv);
            n--;
            if (n == 1) {
                std::cout << i + 1;
                return 0;
            }
        }
    }

    std::cout << -1;

}