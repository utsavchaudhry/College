#!/usr/bin/env python3
import subprocess
import sys

def view_source(file_path):
    try:
        # Open the file with 'less' or another pager
        subprocess.run(['less', file_path])
    except FileNotFoundError:
        # Fallback to 'more' if 'less' is not available
        subprocess.run(['more', file_path])

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: view.py <source_file>")
        sys.exit(1)

    file_path = sys.argv[1]
    view_source(file_path)
