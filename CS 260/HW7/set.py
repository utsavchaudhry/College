class DisjointSet:
    def __init__(self, value):
        self.value = value
        self.parent = self
        self.rank = 0

def Initialize(Values):
    return {value: DisjointSet(value) for value in Values}

def Find(set_system, value):
    if set_system[value].parent != set_system[value]:
        set_system[value].parent = Find(set_system, set_system[value].parent.value)
    return set_system[value].parent

def Merge(set_system, value1, value2):
    root1 = Find(set_system, value1)
    root2 = Find(set_system, value2)
    if root1 != root2:
        if root1.rank < root2.rank:
            root1.parent = root2
        elif root1.rank > root2.rank:
            root2.parent = root1
        else:
            root2.parent = root1
            root1.rank += 1

# Helper function to retrieve the set representative's value for comparison
# Not required, but useful for debugging or integration with other code
def FindRootValue(set_system, value):
    return Find(set_system, value).value
