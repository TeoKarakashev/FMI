#include <iostream>
#include <unordered_map>
#include <climits>
#include <queue>
#include <vector>
#include <algorithm>

int n, m, k;

char office[601][601];
bool visited[601][601];
int sums[601][601];

std::vector<std::pair<int, int>> foods;
int dx[] = { 1,-1,0,0 };
int dy[] = { 0,0,1,-1 };

bool isValid(int x, int y) {
    return x >= 0 && x < n && y >= 0 && y < m;
}


int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);
    std::cin >> n >> m >> k;

    for (int i = 0; i < n; i++) {
        std::cin >> office[i];
    }



    for (int i = 0; i < k; i++) {
        int a, b;
        std::cin >> a >> b;
        foods.push_back({ a - 1, b - 1 });
    }
    long long sum = 0;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            sums[i][j] = INT_MAX;
        }

    }
    

    for (int i = 0; i < foods.size(); i++) {
        int fx = foods[i].first;
        int fy = foods[i].second;

        std::fill(&visited[0][0], &visited[0][0] + sizeof(visited), false);

        visited[fx][fy] = true;
        sums[fx][fy] = 0;

        std::queue<std::pair<int, int>> q;
        q.push({ fx, fy });

        while (!q.empty()) {
            auto curr = q.front();
            q.pop();

            for (int i = 0; i < 4; i++) {
                int nx = curr.first + dx[i];
                int ny = curr.second + dy[i];

                if (isValid(nx, ny) && !visited[nx][ny] && office[nx][ny] != '#') {
                    visited[nx][ny] = true;
                    sums[nx][ny] = std::min(sums[nx][ny], sums[curr.first][curr.second] + 1);
                    q.push({ nx, ny });
                }
            }
        }

    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (office[i][j] == '#') continue;
            sum += sums[i][j];
        }

    }

    std::cout << sum;
}