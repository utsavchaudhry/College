import timeit
import sys
import os

# Naive recursive Fibonacci function
def fibonacci_naive(n):
    if n <= 1:
        return n
    else:
        return fibonacci_naive(n-1) + fibonacci_naive(n-2)

# Initialize memoization array for memoized Fibonacci function
memo = [-1] * 1000
memo[0] = 0
memo[1] = 1

def fibonacci_memo(n):
    if memo[n] == -1:
        memo[n] = fibonacci_memo(n-1) + fibonacci_memo(n-2)
    return memo[n]

def admin_memo_fib(n):
    # Reset memo array between runs
    clear_memo()
    return fibonacci_memo(n)

def clear_memo():
    for i in range(2, len(memo)):
        memo[i] = -1

# Run timing experiments and output to files
def time_fibonacci():
    ranges = {
        '1.out': list(range(5, 45, 5)),
        '2.out': list(range(450, 1000, 50))
    }

    for filename, ns in ranges.items():
        with open(filename, 'w') as f:
            for n in ns:
                # Time the naive Fibonacci function for 1.out
                if filename == '1.out':
                    timer = timeit.Timer(lambda: fibonacci_naive(n))
                # Time the memoized Fibonacci function for 2.out
                else:
                    timer = timeit.Timer(lambda: admin_memo_fib(n))

                # Set number of repetitions
                number = 1000 if filename == '2.out' and n >= 450 else 1

                # Time the execution and write to file
                time = timer.timeit(number=number)
                f.write(f'{n} {time / number}\n')  # Average time per run

    # Generate graphs using gnuplot
    os.system("gnuplot -e \"set terminal png; set output '1.png'; plot '1.out' using 1:2 with linespoints\"")
    os.system("gnuplot -e \"set terminal png; set output '2.png'; plot '2.out' using 1:2 with linespoints\"")

if __name__ == "__main__":
    time_fibonacci()
