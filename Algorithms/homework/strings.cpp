#include <iostream>
#include <string>
std::string s, copy;
int n;

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cin.tie(0);
    std::cout.tie(0);

    std::cin >> s;
    int length = s.length();
    std::cin >> n;
    copy = s;
    for (int i = 0; i < n; i++) {
        int query, index;
        std::cin >> query >> index;
        if (query == 2) {
            s[index] = '#';
        }
        else {
            int leftMax = 0;
            int rightMax = 0;
            int iter = index;
            while (s[iter] == s[index] && iter < length) {
                leftMax++;
                iter++;
            }
            iter = index;
            while (s[iter] == s[index] && iter >= 0) {
                rightMax++;
                iter--;
            }
            std::cout << leftMax + rightMax - 1 << "\n";

        }

    }
}
//std::cout << s;