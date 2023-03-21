#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

vector<int> relativeSortArray(vector<int> &arr1, vector<int> &arr2)
{

    std::vector<int> result = {};
    for (int i = 0; i < arr2.size(); i++)
    {
        for (int j = 0; j < arr1.size(); j++)
        {
            if (arr2[i] == arr1[j])
            {
                result.push_back(arr1[j]);
            }
        }
    }


    std::vector<int> temp = {};
    for (int i = 0; i < arr1.size(); i++)
    {
        bool found = false;
        for (int j = 0; j < arr2.size(); j++)
        {
            if (arr1[i] == arr2[j])
            {
                found = true;
                break;
            }
        }
        if (!found)
        {
            temp.push_back(arr1[i]);
        }
    }
    sort(temp.begin(), temp.end());
    for (int i = 0; i < temp.size(); i++)
    {
        result.push_back(temp[i]);
    }
    return result;
}

int main() {
    std::vector<int> nums = { 1, 4, 2, 5, 3 };
    sumOddLengthSubarrays(nums);
    return 0;
}




