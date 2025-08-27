/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @section Description
 
 Explain what you did in this file:
 */
#ifndef _FIND_TARGET_H_
#define _FIND_TARGET_H_

//You need this for general functional
//programming
#include <functional>

int smaller(int x, int y);
void searchPositiveEvenNumbers(std::function<bool(int)> func);
void searchPositiveOddNumbers(std::function<bool(int)> func);
void searchNegativeEvenNumbers(std::function<bool(int)> func);
void searchNegativeOddNumbers(std::function<bool(int)> func);

/**
 Takes any function that returns bool and takes int as input.
 prints std::cout the smallest (absolute value) number that returns true.
 @param func is the function to test
 */
void findTarget(std::function<bool(int)> func);

//You may add as many additional functions
//or includes to this file as you want.
//You MAY NOT change the prototype for findTarget

#endif
