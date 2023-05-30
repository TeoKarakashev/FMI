#include <iostream>

int arr[62] = { 0 };
int n;
int b;
char num;

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin >> n;

    for (size_t i = 0; i < n; i++) {
        std::cin >> num;
        if (num >= '0' && num <= '9') {
            b = num - '0';
            arr[b]++;
        }
        else if (num >= 'a' && num <= 'z') {
            b = num - 'a' + 10;
            arr[b]++;
        }
        else {
            b = num - 'A' + 36;
            arr[b]++;
        }
    }

    int occurrences = 0;

    for (int i = 0; i < 62; i++) {
        occurrences = arr[i];
        if (occurrences != 0) {
            if (i >= 0 && i <= 9) {
                std::cout << std::string(occurrences, i + '0');
            }
            else if (i >= 10 && i <= 35) {
                std::cout << std::string(occurrences, i - 10 + 'a');
            }
            else {
                std::cout << std::string(occurrences, i - 36 + 'A');
            }
        }
    }
}