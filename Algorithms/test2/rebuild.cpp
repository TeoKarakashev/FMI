#include<iostream>
#include<vector>
#include<algorithm>

struct Edge {
    int from;
    int to;
    int w;
};

int n, m;
int prices[1001];
int parent[1001];
int height[1001];
Edge edges[100001];

void makeSet(int v) {
    parent[v] = v;
    height[v] = 0;
}

int findSet(int v) {
    if (v == parent[v]) {
        return v;
    }
    return parent[v] = findSet(parent[v]);
}

void unionSets(int a, int b) {
    if (a != b) {
        if (height[a] < height[b]) {
            std::swap(a, b);
        }
        parent[b] = a;
        if (height[a] == height[b]) {
            height[a]++;
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> n >> m;
    for (int i = 1; i <= n; i++) {
        std::cin >> prices[i];
    }

    for (int i = 0; i < m; i++) {
        int a, b, c;
        std::cin >> a >> b >> c;
        int priceForReconstruction = 0;
        if (c % 2 == 0) {
            priceForReconstruction = c / 2 * (prices[a] + prices[b]);
        }
        else {
            if (a > b) {
                priceForReconstruction = (c / 2) * (prices[a] + prices[b]) + prices[b];
            }
            else {
                priceForReconstruction = (c / 2) * (prices[a] + prices[b]) + prices[a];
            }
        }
        edges[i] = { a, b, priceForReconstruction };
    }
    for (int i = 0; i < n; i++) {
        makeSet(i);
    }
    int components = n;

    std::sort(edges, edges + m, [](Edge a, Edge b) {
        return a.w < b.w;
        });

    std::vector<std::pair<int, int>> mst;
    long long ans = 0;
    for (int i = 0; i < m && components != 1; i++) {
        int a = findSet(edges[i].from);
        int b = findSet(edges[i].to);
        if (a != b) {
            ans += edges[i].w;
            unionSets(a, b);
            mst.push_back({ edges[i].from, edges[i].to });
            components--;
        }
    }
    std::cout << ans << std::endl;
    std::sort(mst.begin(), mst.end(), [](std::pair<int, int> a, std::pair<int, int> b) {
        if (a.first == b.first) {
            return a.second < b.second;
        }
        return a.first < b.first;
        });

    for (auto x : mst) {
        std::cout << x.first << " " << x.second << std::endl;
    }

}