
# number of lines = 2n
# group of two lines. 
# num of elements = 6.
# xn = ar(n-1)
#2 = ar^3
#
from os import remove
from sys import stdin
import sys

def subsum(array, num,n):
    if (num == 0):
        return True
    if(n == 0 and sum != 0):
        return False
    
    if (array[n-1] > sum):
        return subsum(array, num, n-1)
    return subsum(array, num, n-1) or subsum(array, num-array[n-1], n-1)

def isSubsetSum(set, n, sum):
    visited = []
    # The value of subset[i][j] will be 
    # true if there is a
    # subset of set[0..j-1] with sum equal to i
    subset =([[False for i in range(sum + 1)] 
            for i in range(n + 1)])
      
    # If sum is 0, then answer is true 
    
    for i in range(n+1):
        subset[i][0] = True
        for i in range(1, sum+1):
            subset[0][i] = False
            
            # Fill the subset table in bottom up manner
        for i in range(1, n + 1):
            for j in range(1, sum + 1):
                if j<set[i-1]:
                    subset[i][j] = subset[i-1][j]
                if j>= set[i-1]:
                    subset[i][j] = (subset[i-1][j] or 
                                subset[i - 1][j-set[i-1]])
      
        # uncomment this code to print table 
        for i in range(n + 1):
            for j in range(sum + 1):
                print (subset[i][j], end =" ")
                if subset[j] == False:
                    visited.append("U")
                if subset[j] == True:
                    visited.append("D")
        print(visited)
    return subset[n][sum]       
                

            
def main():
    climbs = []
    numofprobs = 0
    roads = 0
    
    l = [int(x) for x in stdin.readline().split()]
    numofprobs = l[0]
    i = 0
    while i < numofprobs:
        g = [int(x) for x in stdin.readline().split()]
        if i == 0:
            roads = g[0]
        else:
            climbs = g
        i += 1
        
    su = sum(climbs)
    print(isSubsetSum(climbs, len(climbs), 0))

    
if __name__ == "__main__":
    main()