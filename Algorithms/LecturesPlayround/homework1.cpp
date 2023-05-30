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

int counter = 0;
int size = 5;

void print(int*& A) {
    for (int i = 0; i < size; i++) {
        std::cout << A[i] << " ";
    }
    std::cout << std::endl;
}

void someAlg(int*& A, int l, int h) {

    if (A[l] > A[h]) {
        std::swap(A[l], A[h]);
    }
    int t = (h - l + 1) / 3;
    if (t >= 1) {
        someAlg(A, l + t, h);
        someAlg(A, l, h - t);
        someAlg(A, l + t, h);
    }

}



int main() {
    //create test for someAlg
    int* A = new int[size];
    srand(time(NULL));
    for (int i = 0; i < size; i++) {
        A[i] = rand () % 100;
    }
    print(A);
    someAlg(A, 0, size - 1);
    print(A);
    delete[] A;
    return 0;

}