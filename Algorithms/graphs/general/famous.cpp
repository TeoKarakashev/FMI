#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
#include <unordered_map>
#include <cmath>
#include <stack>

int n, k, f, m;
std::unordered_map<std::string, int> people;
std::vector<std::vector<int>> graph;
int famous[1001];

int encode(std::string& name) {
    if (people.count(name)) return -1;
    people[name] = people.size();
    graph.push_back(std::vector<int>());
    return people[name];
}

std::string decode(int id) {
    for (auto& p : people) {
        if (p.second == id) return p.first;
    }
    return "";
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> k >> f >> m;
    std::queue<int> famousPeopleQueue;
    std::set<std::string> famousPeopleRes;

    for (int i = 0; i < f; i++) {
        std::string name;
        std::cin >> name;
        int p = encode(name);
        famous[p] = k;
        famousPeopleQueue.push(p);
        famousPeopleRes.insert(name);
    }

    for (int i = 0; i < m; i++) {
        std::string name, name2;
        std::cin >> name >> name2;
        encode(name);
        encode(name2);
        graph[people[name]].push_back(people[name2]);
        graph[people[name2]].push_back(people[name]);
    }

    while (!famousPeopleQueue.empty()) {
        int p = famousPeopleQueue.front();
        famousPeopleQueue.pop();
        for (int i = 0; i < graph[p].size(); i++) {
            int adj = graph[p][i];
            famous[adj]++;
            if (famous[adj] == k) {
                famousPeopleRes.insert(decode(adj));
                famousPeopleQueue.push(adj);
            }
        }
    }
    for (auto& p : famousPeopleRes) {
        std::cout << p << " ";
    }
}