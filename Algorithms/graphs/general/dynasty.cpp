#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <queue>

using namespace std;

int bfs(vector<vector<int>>& graph, int start) {
    int count = 0;
    queue<int> q;
    q.push(start);
    while (!q.empty()) {
        int curr = q.front();
        q.pop();
        count++;
        for (int adj : graph[curr]) {
            q.push(adj);
        }
    }
    return count - 1;
}

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    int m;
    std::cin >> m;
    unordered_map<string, int> names;
    vector<int> inDegrees;
    vector<vector<int>> graph;
    int count = -1;
    for (int i = 0; i < m; i++) {
        char father[6];
        char son[6];
        std::cin >> father >> son;
        int fatherIndex = -1;
        int sonIndex = -1;
        if (names.count(father)) fatherIndex = names[father];
        else {
            count++;
            fatherIndex = count;
            graph.push_back(vector<int>());
            names[father] = count;
            inDegrees.push_back(0);
        }
        if (names.count(son)) sonIndex = names[son];
        else {
            count++;
            sonIndex = count;
            graph.push_back(vector<int>());
            names[son] = count;
            inDegrees.push_back(0);
        }
        bool isFound = false;
        for (int adj : graph[fatherIndex]) {
            if (adj == sonIndex) {
                isFound = true;
                break;
            }
        }
        if (isFound) continue;
        inDegrees[sonIndex]++;
        if (inDegrees[sonIndex] > 1) {
            std::cout << son;
            return 0;
        }
        graph[fatherIndex].push_back(sonIndex);
    }
    int maxIndex = -1;
    int maxDynasty = -1;
    for (int i = 0; i < graph.size(); i++) {
        if (inDegrees[i] == 0) {
            int countOfDynasty = bfs(graph, i);
            if (countOfDynasty >= maxDynasty) {
                maxIndex = i;
                maxDynasty = countOfDynasty;
            }
        }
    }
    for (auto p : names) {
        if (p.second == maxIndex) {
            std::cout << p.first;
            break;
        }
    }
}