# prob1.py
import sys

def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n-1) + fibonacci(n-2)
'''
if __name__ == "__main__":
    n = int(sys.argv[1])
    print(fibonacci(n))
   ''' 
print(fibonacci(45))
