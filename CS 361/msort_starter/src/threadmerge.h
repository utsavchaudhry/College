/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date 2021-2022
 @section Description
 
 This file describes the threaded merge sort. Threads are used to speed up the classic merge sort.
 */

//--You may add to this file, but you may not change the templates provided--
#ifndef _THREAD_MERGE_H_
#define _THREAD_MERGE_H_

/**
 Merge Sort array in place using maximum number of threads provided. If number of elements is smaller than number of threads, extraneous threads are not spanned.
 @param A is the array to sort
 @param size is the number of elements in the array
 @param threadCount is the maximum number of threads to use when sorting.
 
 */
void tmergesort(int* A, int size, int threadCount);

/**
 Merge Sort Array values between index start and stop
 @param A is the array to sort
 @param start is the first index to sort
 @param stop is the last index to sort
 */
void tmsortSec(int* A, int start, int stop);
/**
 Merge two sorted sections of the array. Specifically,
 merge A[start to middle] an A[middle+1 to stop].
 @param A is the array to merge elements in
 @param start is the first value in the first section
 @param middle is the last value in the first section
 @param stop is the last element in the second section
 */
void tmerge(int* A, int start, int middle, int stop);

#endif
