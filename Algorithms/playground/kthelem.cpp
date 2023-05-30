#include <iostream>
#include <algorithm>

int main() {
    //make an array with some values
    int arr[10] = { 3, 1, 4, 2, 5, 8, 7, 6, 9, 10 };
    auto m = arr + 2;
    std::nth_element(arr, m, arr + 10);
    std::cout << *m << std::endl;
}