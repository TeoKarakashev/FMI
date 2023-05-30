#include <cstdint>
#include <functional>
#include <iostream>
#include <queue>
#include <vector>
#include <algorithm>
//#include <cstdio>

using namespace std;

int n,m,k;
char graph[602][602];
int results[602][602];
vector<pair<int, int>> food;

int dx[] = {1,-1,0,0};
int dy[] = {0,0,1,-1};
vector<int> ansx;
vector<int> ansy;

void matrPrint()  {
	for (size_t i = 1; i <= n; i++) {
		for (size_t j = 1; j <= m; j++) {
			printf("%c", graph[i][j]);
		}
		printf("\n");
	}
}
void resPrint() {
	for (size_t i = 1; i <= n; i++) {
		for (size_t j = 1; j <= m; j++) {
			printf("%d", results[i][j]);
		}
		printf("\n");
	}
}
bool isCorrect(int x, int y) {
	return x >= 1  && y >= 1 && x <= n && y <= m && graph[x][y] == '.';
}

int main() {
	scanf("%d%d%d", &n, &m, &k);
	for (size_t i = 1; i <= n; i++) {
		scanf("%s", &graph[i][1]);
		/* scanf("%s", graph); */
	}
	for (size_t i = 0; i < k; i++) {
		int fx = -1, fy = -1;
		scanf("%d%d", &fx, &fy);
		food.push_back({fx, fy});
	}
	
	/* printf("Matrix:\n"); */
	/* matrPrint(); */
	bool isSetOnce = false;
	
	for(auto [fx,fy] : food) {
		
		vector<vector<bool>> isVisited;
		isVisited.resize(601, {});
		for (size_t i = 0; i < 601 ; i++) {
			isVisited[i].resize(601, false);
		}
		isVisited[fx][fy] = true;
		
		queue<pair<int, int>> q;
		q.push({fx,fy});
		int dist = 0;
		results[fx][fy] = dist;
		int remainder = 1;
		
		/* printf("Food { %d , %d }\n", fx, fy); */
		
		while(!q.empty()) {
			int curX = q.front().first, curY = q.front().second;
			q.pop();
			remainder--;
			
			for (size_t i = 0; i < 4; i++) {
				int nx = curX + dx[i];
				int ny = curY + dy[i];
				
				if(!isVisited[nx][ny] && isCorrect(nx, ny)) {
					/* printf("Path: { %d , %d } -> %c\n", nx, ny, graph[nx][ny]); */
					isVisited[nx][ny] = true;
					
					if(!isSetOnce) {
						ansx.push_back(nx);
						ansy.push_back(ny);
						results[nx][ny] = dist + 1;
					}
					if(dist < results[nx][ny]) {
						results[nx][ny] = dist + 1;
					}
					
					q.push({nx,ny});
				}
			}
			
			if(remainder == 0) {
				remainder = q.size();
				dist++;
			}
		}
		isSetOnce = true;
		/* printf("RESULTS:\n"); */
		/* resPrint(); */
		/* printf("DIST: %d\n", dist); */
	}
	
	int length = ansx.size();
	long long res = 0;
	for (size_t i = 0; i < length; i++) {
		res += results[ansx[i]][ansy[i]];
	}
	printf("%lli\n", res);
}