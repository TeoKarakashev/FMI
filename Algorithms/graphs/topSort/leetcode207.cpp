#include <iostream>
#include <vector>
#include <unordered_map>

using namespace std;

unordered_map<int, vector<int>> adj;
vector<bool> visited, onCurPath;

bool dfs(int i) {
    visited[i] = true;
    onCurPath[i] = true;

    for (auto x : adj[i]) {
        if ((!visited[x] && dfs(x)) || onCurPath[x]) {
            return true;
        }
    }

    onCurPath[i] = false;

    return false;
}


bool canFinish(int numCourses, vector<vector<int>>& prerequisites) {
    for (auto x : prerequisites) {
        adj[x[1]].push_back(x[0]);
    };
    visited = vector<bool>(numCourses);
    onCurPath = vector<bool>(numCourses);

    for (int i = 0; i < numCourses; i++) {
        if (!visited[i] && dfs(i)) {
            return false;
        }
    }
    return true;
}


int main() {
    int numCourses = 2;
    vector<vector<int>> prerequisites = { {1, 0} };
    cout << canFinish(numCourses, prerequisites) << endl;
    return 0;
}