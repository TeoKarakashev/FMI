#include<iostream>
#include<vector>
#include<algorithm>

struct Student {
    std::string name;
    int fn;
    int points;

    bool operator<(const Student& other) {
        return points < other.points || (points == other.points && fn < other.fn);
    }

    Student(std::string name, int fn, int points) {
        this->name = name;
        this->fn = fn;
        this->points = points;
    }
};

int main() {
    std::ios_base::sync_with_stdio(false);
    int n, q;
    std::cin >> n >> q;

    std::string name;
    int fn;
    int points;
    int pointsFor3;
    std::vector<Student> students;
    std::vector<int> pointsFor3Vec;



    for (size_t i = 0; i < n; i++) {
        std::cin >> name;
        std::cin >> fn;
        std::cin >> points;
        students.push_back(Student(name, fn, points));
    }

    std::sort(students.begin(), students.end());

    for (size_t i = 0; i < q; i++) {
        std::cin >> pointsFor3;
        auto student = std::lower_bound(students.begin(), students.end(), pointsFor3, [](const Student& student1, int number) {
            return student1.points < number;
            });
        std::cout << student->name << " " << student->fn << std::endl;
    }

}