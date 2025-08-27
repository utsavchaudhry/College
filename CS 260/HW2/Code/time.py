import timeit
from cell import Cell

def list_concat_copy(A, B):
    """ Non-destructively concatenates two lists by creating copies """
    def copy_list(L):
        """ Copies a linked list iteratively """
        if L is None:
            return None
        head = Cell(L.data)
        current_copy = head
        current = L.next
        while current:
            current_copy.next = Cell(current.data)
            current_copy = current_copy.next
            current = current.next
        return head

    A_copy = copy_list(A)
    B_copy = copy_list(B)
    return list_concat(A_copy, B_copy)

def list_concat(A, B):
    """ Concatenates two lists destructively """
    if A is None:
        return B
    if B is None:
        return A
    tail = A
    while tail.next is not None:
        tail = tail.next
    tail.next = B
    return A

def create_list(size):
    """ Creates a linked list of the specified size """
    if size <= 0:
        return None
    head = current = Cell(0)
    for i in range(1, size):
        current.next = Cell(i)
        current = current.next
    return head

def time_function(func, size):
    """ Times a given function with lists of a specified size """
    A = create_list(size)
    B = create_list(size)
    timer = timeit.Timer(lambda: func(A, B))
    return timer.timeit(number=1)

# Define the range of sizes to test
sizes = [1000 * i for i in range(1, 16)]  # 1000 to 15000, in steps of 1000

# Record the timings
results = []
for size in sizes:
    time_concat = time_function(list_concat, size)
    time_concat_copy = time_function(list_concat_copy, size)
    results.append((size, time_concat, time_concat_copy))

# Save the results to a file for plotting
with open('data.out', 'w') as file:
    for size, time_concat, time_concat_copy in results:
        file.write(f"{size}\t{time_concat}\t{time_concat_copy}\n")

print("Timing results saved to 'data.out'")
