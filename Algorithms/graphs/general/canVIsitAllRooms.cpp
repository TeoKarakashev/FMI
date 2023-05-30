#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <unordered_set>

void dfs(std::vector<std::vector<int>>& rooms, std::vector<int>& visited, int room) {
    if (visited[room]) {
        return;
    }
    visited[room] = true;
    for (int i = 0; i < rooms[room].size(); i++) {
        dfs(rooms, visited, rooms[room][i]);
    }
}

bool canVisitAllRooms(std::vector<std::vector<int>>& rooms) {
    std::vector<int> visited(rooms.size(), 0);

    dfs(rooms, visited, 0);

    for (int i = 0; i < rooms.size(); i++) {
        if (!visited[i]) {
            return false;
        }
    }
    return true;
}

int main() {
    std::vector<std::vector<int>> rooms = { {1}, {2}, {3}, {} };
    std::cout << canVisitAllRooms(rooms);
}