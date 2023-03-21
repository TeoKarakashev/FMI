#include <iostream>
#include <algorithm>
#include <vector>
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

                //create a 
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

vector<int> twoSum(vector<int>& nums, int target) {

        std::vector<int> res;
        int startInd = 0;
        int endInd = nums.size() - 1;
        std::vector <int> temp = nums;
        sort(temp.begin(), temp.end());


        while(startInd < endInd){
            int sumNums = temp[startInd] + temp[endInd];
            if(sumNums == target){\
                int firstNum = temp[startInd];
                int secondNum = temp[endInd];
                for(int i = 0; i < nums.size(); i++){
                    if(nums[i] == firstNum){
                        res.push_back(i);
                        break;
                    }
                }
                for(int i = 0; i < nums.size(); i++){
                    if(nums[i] == secondNum && i != res[0]){
                        res.push_back(i);
                        break;
                    }
                }
                return res;
            }
            else if(sumNums > target){
                endInd--;
            }
            else{
                startInd++;
            }
        }

        return res;
    }


bool isPrime(int num)
{
    if (num == 1)
    {
        return false;
    }
    for (int i = 2; i < num; i++)
    {
        if (num % i == 0)
        {
            return false;
        }
    }
    return true;
} 
int main()
{
    std::vector<int> arr1 = {2, 3, 1, 3, 2, 4, 6, 7, 9, 2, 19};
    std::vector<int> arr2 = {2, 1, 4, 3, 9, 6};
    std::vector<int> result = relativeSortArray(arr1, arr2);

    for (int i = 0; i < result.size(); i++)
    {
        cout << result[i] << " ";
    }
}