#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int max_sequence_sum(const vector<int>& sequence) {
    int n = sequence.size();
    
    // Инициализиране на динамичното програмиране с мемоизация
    vector<int> dp(n + 1, 0);
    
    // Пресмятане на резултатите за всеки индекс i
    for (int i = 1; i <= n; i++) {
        // Най-голямата сума до момента
        int max_sum = INT_MIN;
        
        // Пробваме всички възможни начини за умножение
        for (int j = 1; j <= i; j++) {
            if (j == i || (sequence[j - 1] != 2 * sequence[i - 1] && sequence[j - 1] != 3 * sequence[i - 1])) {
                // Ако не можем да умножим две съседни числа с 3, пресмятаме сумата
                int current_sum = dp[j - 1] + sequence[i - 1];
                max_sum = max(max_sum, current_sum);
            }
        }
        
        // Запазване на максималната сума за текущия индекс
        dp[i] = max_sum;
    }
    
    // Връщане на максималната сума за цялата редица
    return dp[n];
}

int main() {
    // Примерно извикване на функцията
    vector<int> sequence = {1, 2, 3, 4, 5};
    int max_sum = max_sequence_sum(sequence);
    cout << "Максималната сума е: " << max_sum << endl;
    
    return 0;
}