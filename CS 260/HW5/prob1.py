import time
import random

def downheap(arr, n, i):
    largest = i
    left = 2 * i + 1
    right = 2 * i + 2

    if left < n and arr[largest] < arr[left]:
        largest = left
    if right < n and arr[largest] < arr[right]:
        largest = right

    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        downheap(arr, n, largest)

def make_heap(arr, n):
    for i in range(n // 2 - 1, -1, -1):
        downheap(arr, n, i)

sizes = [10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000]
results = []

for n in sizes:
    arr = [random.randint(1, 1000) for _ in range(n)]
    start_time = time.time()
    make_heap(arr, n)
    end_time = time.time()
    results.append((n, end_time - start_time))

print(" n      | T(n)")
print("--------|---------")
for n, t in results:
    print(f"{n:<8}| {t:.6f}")
