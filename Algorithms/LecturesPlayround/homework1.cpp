#include <iostream>

int foo(int a) {
    int i, x = 6, y = 1, z = 0;
    for (i = 0; i < a; i++) {
        z += y;
        y += x;
        x += 6;
    }
    return z;
}

void someAlg(int*& A, int l, int h) {
    if (l < h) {
        if (A[l] > A[h]) {
            std::swap(A[l], A[h]);
        }
        int t = (h - l + 1) / 3;
        if (t >= 1) {
            someAlg(A, l, h - t);
            someAlg(A, l + t, h);
            someAlg(A, l, h - t);
        }
    }
}


int main() {
    int* arr = new int[10];
    srand(time(NULL));
    for (int i = 0; i < 10; i++) {
        arr[i] = rand() % 100;
    } 
    for (int i = 0; i < 10; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout<<std::endl;
    someAlg(arr, 0, 4);
    for (int i = 0; i < 10; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
    return 0;

}