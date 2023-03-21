#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

vector<vector<int>> sortTheStudents(vector<vector<int>> score, int k) {
    sort(score.begin(), score.end(), [&k](vector<int>& a, vector<int>& b) {
        return a[k] > b[k];
        });

    return score;
}


int evil(int a, int b) {
    return a > b;
}

int main() {
    long n;
    cin >> n;
    bool arr[n] = { 0 };
    int counter = 0;
    int k;
    cin >> k;
    for (size_t i = 0; i < k; i++) {
        int a, b;
        cin >> a >> b;
        for (size_t j = a; j <= b; j++) {
            if (arr[j] == 0) {
                arr[j] = 1;
                counter++;
            }
        }
    }
    std::cout << counter;
}