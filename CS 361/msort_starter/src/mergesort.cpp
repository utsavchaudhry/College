/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/17/24
 @section Description
 
 Classic textbook Merge Sort Algorithm.
 */

#include <cmath>
#include "mergesort.h"

void mergesort(int* A, int size) {
    
    msortSec(A, 0, size - 1);
    
}

void msortSec(int* A, int start, int stop) {
    
    // Base case: If the subarray has one or no elements, it's already sorted
    if (start >= stop) {
        return;
    }
    
    // Calculate the middle index of the current subarray
    int middle = start + std::floor((stop - start) / 2.0f);
    
    // Recursively sort the left half of the subarray
    msortSec(A, start, middle);
    // Recursively sort the right half of the subarray
    msortSec(A, middle + 1, stop);
    
    // Merge the two sorted halves
    merge(A, start, middle, stop);
    
}

void merge(int* A, int start, int middle, int stop) {
    
    // Total number of elements to merge
    int size = stop - start + 1;
    
    // Create an auxiliary array to hold the elements to be merged
    int* Aux = new int[size];
    
    // Copy the relevant elements from A into the auxiliary array
    for (int i = 0; i < size; i++) {
        Aux[i] = A[start + i];
    }
    
    // Define indices for the left and right halves within the auxiliary array
    int Aux_middle = (middle - start);
    int Aux_stop = (stop - start);
    
    int i = 0;                      // Initial index of the left half
    int j = Aux_middle + 1;         // Initial index of the right half
    
    // Merge the left and right halves back into the original array A
    for (int k = start; k < stop + 1; k++) {
        if (i > Aux_middle) {
            // Left half is exhausted; take element from the right half
            A[k] = Aux[j];
            j++;
        } else if (j > Aux_stop) {
            // Right half is exhausted; take element from the left half
            A[k] = Aux[i];
            i++;
        } else if (Aux[j] > Aux[i]) {
            // Current element in left half is smaller or equal; take it
            A[k] = Aux[i];
            i++;
        } else {
            // Current element in right half is smaller; take it
            A[k] = Aux[j];
            j++;
        }
    }
    
    delete[] Aux;                   // Deallocate the auxiliary array to prevent memory leaks
    
}
