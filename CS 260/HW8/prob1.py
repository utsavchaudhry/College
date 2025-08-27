#!/usr/bin/env python

import sys

def read_graph():
    graph = {}
    for line in sys.stdin:
        parts = line.split()
        if not parts:
            continue  # Skip empty lines
        node = int(parts[0])
        edges = []
        for edge in parts[1:]:
            target, weight = map(int, edge.split(','))
            edges.append((target, weight))
        graph[node] = edges
    return graph

def floyds_algorithm(graph, n):
    # Initialize distance and predecessor matrices
    dist = [[float('inf') for _ in range(n)] for _ in range(n)]
    pred = [[None for _ in range(n)] for _ in range(n)]
    
    # Set distance to self as 0
    for i in range(n):
        dist[i][i] = 0
    
    # Populate initial distances and predecessors from graph
    for node, edges in graph.items():
        for target, weight in edges:
            dist[node][target] = weight
            dist[target][node] = weight  # Since the graph is undirected
            pred[node][target] = node
            pred[target][node] = target
    
    # Floyd's algorithm
    for k in range(n):
        for i in range(n):
            for j in range(n):
                if dist[i][k] + dist[k][j] < dist[i][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]
                    pred[i][j] = pred[k][j]
    
    return dist, pred

def print_matrix(matrix):
    for row in matrix:
        for val in row:
            if val == float('inf'):
                print(" INF".rjust(4), end="")
            else:
                print(f"{val:4d}", end="")
        print()
    print()

def main():
    graph = read_graph()
    n = max(graph) + 1  # Assuming nodes are labeled from 0 to n-1
    dist, pred = floyds_algorithm(graph, n)
    
    print("Distance Matrix:")
    print_matrix(dist)
    
    print("Predecessor Matrix:")
    for i in range(n):
        for j in range(n):
            if pred[i][j] is None:
                print("  -", end="")
            else:
                print(f"{pred[i][j]:3d}", end="")
        print()
    print()

if __name__ == "__main__":
    main()
