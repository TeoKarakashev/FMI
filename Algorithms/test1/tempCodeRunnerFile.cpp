#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>

int n, k, s;
int popcorns[100001];

bool good(int x) {
    int currentSeconds = 0;
    int packages = 0;
    int personsCounter = 0;
    int currPackage = 0;
    for (int i = 0; i < n; i++) {
        int temp = std::ceil((double) popcorns[i] / s);
        if (temp > x) {
            return false;
        }
        if (currentSeconds + temp > x) {
            i--;
            currentSeconds = 0;
        }
        else {
            packages++;
            currentSeconds += temp;
        }
        if (currentSeconds - temp == 0) {
            personsCounter++;
        }
    }

    return personsCounter <= k && packages >= n;
}


int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin >> n >> k >> s;

    for (int i = 0; i < n; i++) {
        std::cin >> popcorns[i];
    }

    int l = 0;
    int r = INT_MAX;
    int m;
    while (l <= r) {
        m = l + (r - l) / 2;
        if (good(m)) {
            r = m - 1;
        }
        else {
            l = m + 1;
        }
    }
    std::cout << l;

}