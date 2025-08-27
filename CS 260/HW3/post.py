#!/usr/bin/env python3
from lexer import *

class TreeNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

def postorder(expression):
    stack = []
    for token in expression.split():
        if token.isdigit():
            stack.append(TreeNode(token))
        else:
            right = stack.pop()
            left = stack.pop()
            node = TreeNode(token)
            node.left = left
            node.right = right
            stack.append(node)
    return stack[0]

def pre_order(node):
    if node:
        print(node.value, end=' ')
        pre_order(node.left)
        pre_order(node.right)

def in_order(node):
    if node:
        in_order(node.left)
        print(node.value, end=' ')
        in_order(node.right)

def post_order(node):
    if node:
        post_order(node.left)
        post_order(node.right)
        print(node.value, end=' ')

def evaluate(node):
    if node.value.isdigit():
        return int(node.value)
    else:
        left_val = evaluate(node.left)
        right_val = evaluate(node.right)
        if node.value == '+':
            return left_val + right_val
        elif node.value == '-':
            return left_val - right_val
        elif node.value == '*':
            return left_val * right_val
        elif node.value == '/':
            return left_val / right_val

while get_expression():
    expression = ''
    t = get_next_token()
    while t:
        expression += t + ' '
        t = get_next_token()
    expression = expression.strip()

    root = postorder(expression)

    print("pre:", end=' ')
    pre_order(root)
    print("\n in:", end=' ')
    in_order(root)
    print("\npost:", end=' ')
    post_order(root)
    print("\neval:", evaluate(root))
    print("\n")
