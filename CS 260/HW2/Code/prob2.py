from cell import Cell

def create_list(elements):
    """ Helper function to create a linked list from a list of elements """
    if not elements:
        return None
    head = Cell(elements[0])
    current = head
    for element in elements[1:]:
        current.next = Cell(element)
        current = current.next
    return head

def list2string(L):
    """ Function to convert a linked list to a string representation """
    if L is None:
        return '()'
    rv = '('
    while L.next is not None:
        rv += str(L) + ', '
        L = L.next
    rv += str(L) + ')'
    return rv

def list_concat_copy(A, B):
    """ Non-destructively concatenates two lists by creating copies """
    def copy_list(L):
        if L is None:
            return None
        return Cell(L.data, copy_list(L.next))

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

# Create lists A and B
A = create_list(['a', 'b', 'c'])
B = create_list([11, 12, 13])

# Demonstrate list_concat_copy
concatenated_copy = list_concat_copy(A, B)

# Print the results
print("Concatenated Copy:", list2string(concatenated_copy))
print("Original A:", list2string(A))
print("Original B:", list2string(B))
