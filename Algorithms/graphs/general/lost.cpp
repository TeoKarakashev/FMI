#include<iostream>
#include<unordered_map>
#include<vector>
#include<queue>

int n;
std::unordered_map<std::string, int> names;

std::unordered_map<int, int> parent;
std::unordered_map<int, int> height;

int findSet(int x) {
    if (parent[x] == x) {
        return x;
    }
    return parent[x] = findSet(parent[x]);
}

int unionSets(int u, int v) {
    if (height[v] > height[u]) {
        std::swap(u, v);
    }
    else if (height[u] == height[v]) {
        height[u]++;
    }
    parent[v] = parent[u];
}

void encode(std::string name) {
    if (names.count(name)) return;
    int code = names.size();
    names[name] = code;
    parent[code] = code;
    height[code] = 0;
}

std::string decode(int code) {
    for (auto it = names.begin(); it != names.end(); it++) {
        if (it->second == code) return it->first;
    }
    return "";
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n;
    names.reserve(n);
    graph.reserve(n);
    std::string start, finish;
    std::cin >> start >> finish;
    encode(start);
    encode(finish);
    for (int i = 0; i < n - 1; i++) {
        std::string from, to;
        std::cin >> from >> to;
        encode(from);
        encode(to);
        graph[names[from]].push_back(names[to]);
        graph[names[to]].push_back(names[from]);
    }




}