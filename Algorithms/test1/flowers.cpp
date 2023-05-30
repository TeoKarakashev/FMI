#include <iostream>
#include <algorithm>


int n, k;
int originalPrices[500000];

int main() {
    std::ios_base::sync_with_stdio(false);

    std::cin >> n >> k;
    long long sum = 0;
    int count = 0;
    int a;
    for (int i = 0; i < n; i++) {
        std::cin >> originalPrices[i];
    }

    if (n <= k) {
        for (int i = 0; i < n; i++) {
            sum += originalPrices[i];
        }
        std::cout << sum;
        return 0;
    }



    std::sort(originalPrices, originalPrices + n);
    int multiplier = 1;
    int counter = 0;
    for (int i = n - 1; i >= 0; i--) {
        if (counter == k) {
            multiplier++;
            counter = 0;
        }
        sum += multiplier * originalPrices[i];
        counter++;
    }
    std::cout << sum;
}