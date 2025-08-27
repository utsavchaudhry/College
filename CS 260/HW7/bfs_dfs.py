Graph = {
    'A': ['B', 'C', 'D'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['A', 'B', 'E'],
    'E': ['B', 'D', 'G'],
    'F': ['C'],
    'G': ['E']
}

from collections import deque

def BFS(graph, start):
    visited = set()
    queue = deque([start])
    visited.add(start)
    while queue:
        vertex = queue.popleft()
        print(vertex, end=" ")
        for neighbor in graph[vertex]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)
                
def DFS(graph, start, visited=None):
    if visited is None:
        visited = set()
    visited.add(start)
    print(start, end=" ")
    for neighbor in graph[start]:
        if neighbor not in visited:
            DFS(graph, neighbor, visited)
            
print("BFS Traversal: ", end="")
BFS(Graph, 'A')
print("\nDFS Traversal: ", end="")
DFS(Graph, 'A')

