/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/17/24
 @section Description
 
 Classic textbook Merge Sort Algorithm using threads.
 */

#include <vector>
#include <thread>
#include "threadmerge.h"
#include "mergesort.h"

void tmergesort(int* A, int size, int threadCount) {
    
    // If only one thread is specified or the array size is less than the number of threads, use the regular mergesort
    if (threadCount == 1 || size < threadCount) {
        mergesort(A, size);
        return;
    }
    
    int elementsPerThread = size / threadCount;
    
    std::vector<std::thread> myThreads(0);
    
    // Launch threads to sort individual sections of the array
    for (int i = 0; i < threadCount; i++) {
        int stop = (elementsPerThread * (i + 1)) - 1;
        // Ensure the last thread processes any remaining elements
        if (i == threadCount - 1) {
            stop = size - 1;
        }
        myThreads.emplace_back(std::thread(tmsortSec, A, elementsPerThread * i, stop));
    }
    
    for (auto& t:myThreads) {
        t.join();
    }
    
    // Merge the sorted subarrays
    int currentSize = elementsPerThread;
    while (currentSize < size) {
        int left = 0;
        // Merge subarrays in pairs
        while (left + currentSize < size) {
            int mid = left + currentSize - 1;
            int right = std::min(left + 2 * currentSize - 1, size - 1);
            // Merge the two subarrays from left to mid and mid+1 to right
            tmerge(A, left, mid, right);
            left += 2 * currentSize;
        }
        currentSize *= 2;
    }
    
}

void tmsortSec(int* A, int start, int stop) {
    
    // Base case: if the subarray has one or no elements
    if (start >= stop) {
        return;
    }
    
    int middle = start + std::floor((stop - start) / 2.0f);
    
    // Recursively sort the left half
    tmsortSec(A, start, middle);
    // Recursively sort the right half
    tmsortSec(A, middle + 1, stop);
    
    // Merge the two halves
    tmerge(A, start, middle, stop);
    
}

void tmerge(int* A, int start, int middle, int stop) {
    
    // Total number of elements to merge
    int size = stop - start + 1;
    
    // Create an auxiliary array to store elements to be merged
    int* Aux = new int[size];
    
    for (int i = 0; i < size; i++) {
        Aux[i] = A[start + i];
    }
    
    // Indices for the left and right halves in the auxiliary array
    int Aux_middle = (middle - start);              // Index for left half
    int Aux_stop = (stop - start);                  // Index for right half
    
    int i = 0;
    int j = Aux_middle + 1;
    
    for (int k = start; k < stop + 1; k++) {
        if (i > Aux_middle) {
            // Left half is exhausted; take from right half
            A[k] = Aux[j];
            j++;
        } else if (j > Aux_stop) {
            // Right half is exhausted; take from left half
            A[k] = Aux[i];
            i++;
        } else if (Aux[j] > Aux[i]) {
            // Element in left half is smaller or equal; take it
            A[k] = Aux[i];
            i++;
        } else {
            // Element in right half is smaller; take it
            A[k] = Aux[j];
            j++;
        }
    }
    
    // Delete the auxiliary array to free memory
    delete[] Aux;
    
}
