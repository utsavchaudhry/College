/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @section Description
 
 Explain what you did in this file:
 */
//For I/O
#include <iostream>
//Get the prototypes
#include "find_target.h"
//You may include other libraries
#include <cstdlib>
#include <thread>
#include <mutex>

int smallest;
bool firstSet;

int smaller(int x, int y) {
    if (abs(x) < abs(y)) {
        return x;
    }
    if (abs(y) < abs(x)) {
        return y;
    } else {
        return abs(x);
    }
}

void setSmallest(int i) {
    
    static std::mutex setSmallestLk;
    
    {std::lock_guard<std::mutex> lk(setSmallestLk);
        if (!firstSet) {
            smallest = i;
            firstSet = true;
        } else {
            smallest = smaller(i, smallest);
        }
    }
    
}

void searchPositiveEvenNumbers(std::function<bool(int)> func) {
    for (int i = 0; i<=99998; i+=2) {
        if (func(i)) {
            setSmallest(i);
        }
    }
}

void searchPositiveOddNumbers(std::function<bool(int)> func) {
    for (int i = 1; i<=99999; i+=2) {
        if (func(i)) {
            setSmallest(i);
        }
    }
}

void searchNegativeEvenNumbers(std::function<bool(int)> func) {
    for (int i = -2; i>=-99998; i-=2) {
        if (func(i)) {
            setSmallest(i);
        }
    }
}

void searchNegativeOddNumbers(std::function<bool(int)> func) {
    for (int i = -1; i>=-99999; i-=2) {
        if (func(i)) {
            setSmallest(i);
        }
    }
}

//Rewrite this function to solve the problem.
//You may not change the prototype, only implementation.
void findTarget(std::function<bool(int)> func) {
    
    firstSet = false;
    
    std::thread pe(searchPositiveEvenNumbers, func);
    std::thread po(searchPositiveOddNumbers, func);
    std::thread ne(searchNegativeEvenNumbers, func);
    std::thread no(searchNegativeOddNumbers, func);
    
    pe.join();
    po.join();
    ne.join();
    no.join();
    
    std::cout << "The smallest (abs val) answer that makes the function true is "
    << smallest << std::endl;
    return;
}

//Add as many other functions as you need.
