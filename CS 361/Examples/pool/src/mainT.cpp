/**
 @mainpage
 @section Overview
 
 See entire slide deck for more information!
 */
/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
Use threads to play all games of Tic-Tac-Toe
 */

#include <iostream>
#include <thread>
#include <mutex>
#include <queue>
#include <vector>
#include "task.h"
#include "searchTask.h"
#include "move.h"
#include "threadPool.h"

/**
 Use a thread pool to solve problem.
 @return 0 always 
 */
int main(void){
	//Make a Thread Pool
    threadPool* pool = new threadPool();
    //A game starts with no moves made
    std::vector<move> actionList;
    //Put tasks into a queue
    pool->addWork(new searchTask(actionList));
    //Start up the pool
    pool->start();
    //Exit
    return 0;
}

