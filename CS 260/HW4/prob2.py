# prob2.py
import sys

# Initialize memoization array with -1, denoting uncalculated Fibonacci numbers
memo = [-1] * 1000
memo[0] = 0  # F(0)
memo[1] = 1  # F(1)

def fibonacci(n):
    if memo[n] == -1:
        memo[n] = fibonacci(n-1) + fibonacci(n-2)
    return memo[n]

if __name__ == "__main__":
    n = int(sys.argv[1])
    print(fibonacci(n))
    # Clear the memo array for future runs
    for i in range(2, n+1):
        memo[i] = -1
