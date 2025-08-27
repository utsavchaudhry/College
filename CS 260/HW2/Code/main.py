from cell import Cell
from cell import list2string
import timeit
import timeit
import sys
from cell import Cell

def create_list(size):
    head = Cell(0)
    current = head
    for i in range(1, size):
        current.next = Cell(i)
        current = current.next
    return head

def time_function(func, size):
    A = create_list(size)
    B = create_list(size)
    timer = timeit.Timer(lambda: func(A, B))
    return timer.timeit(number=1)

def list_concat(A, B):
    if A is None:
        return B
    if B is None:
        return A

    # Find the tail of A
    tail = A
    while tail.next is not None:
        tail = tail.next

    # Concatenate B at the end of A
    tail.next = B
    return A

def list_concat_copy(A, B):
    def copy_list(L):
        if L is None:
            return None
        return Cell(L.data, copy_list(L.next))

    # Copy lists
    A_copy = copy_list(A)
    B_copy = copy_list(B)

    return list_concat(A_copy, B_copy)

# Testing list_concat
A = Cell('a', Cell('b', Cell('c')))
B = Cell(11, Cell(12, Cell(13)))
concatenated_list = list_concat(A, B)
print("Concatenated List:", list2string(concatenated_list))

# Testing list_concat_copy
A = Cell('a', Cell('b', Cell('c')))
B = Cell(11, Cell(12, Cell(13)))
concatenated_copy = list_concat_copy(A, B)
print("Concatenated Copy:", list2string(concatenated_copy))
print("Original A:", list2string(A))
print("Original B:", list2string(B))
