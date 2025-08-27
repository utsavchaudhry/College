# Initialize the memoization array with -1 for uncalculated values.
# The first two values are known: F(0) = 0, F(1) = 1.
memo = [-1] * 1000
memo[0] = 0
memo[1] = 1

def fibonacci(n):
    # If the value has not been computed, compute it recursively.
    if memo[n] == -1:
        memo[n] = fibonacci(n-1) + fibonacci(n-2)
    # Return the memoized value.
    return memo[n]

# This function resets the memoization array for new runs.
def clear_memo():
    for i in range(2, len(memo)):
        memo[i] = -1
