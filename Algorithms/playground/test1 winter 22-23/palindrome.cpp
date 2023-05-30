#include <iostream>

int arr[26];

std::string createPalindrome() {
    int counterOdd = 0;
    int indexOdd = -1;
    int wordIndex = 0;
    std::string res;
    for (int i = 0; i <= 25; i++) {
        if (arr[i] % 2 == 1) {
            counterOdd++;
            indexOdd = i;
            if (counterOdd > 1) {
                return "IMPOSSIBLE";
            }
        }
    }
    for (int i = 0; i <= 25; i++) {
        res += std::string(arr[i] / 2, i + 'a');
    }
    if (indexOdd != -1) {
        res += indexOdd + 'a';
    }
    for (int i = 25; i >= 0; i--) {
        res += std::string(arr[i] / 2, i + 'a');
    }

    return res;
}


int main() {
    std::ios_base::sync_with_stdio(false);
    std::string s;
    std::cin >> s;
    for (size_t i = 0; i < s.size(); i++) {
        arr[s[i] - 'a']++;
    }
    std::string res = "";
    if (!s.empty()) {
        res = createPalindrome();
    }
    std::cout<<res;
}
