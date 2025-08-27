/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
Use sequential solution to play all games of Tic-Tac-Toe
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
 Test the features of the thread pool in a sequential setting.
 @return 0 always
 */
int main(void){
	//Make a Queue of Tasks
    std::queue<task*>* toDoList = new std::queue<task*>();
    //A game starts with no moves made
    std::vector<move> actionList;
    //Put tasks into a queue
    toDoList->push(new searchTask(actionList));
    //Do all the tasks
    while(!toDoList->empty()){
        task* t = toDoList->front();
        toDoList->pop();
        //Run the task
        std::queue<task*>* more = t->runTask();
        while(!more->empty()){
			toDoList->push(more->front());
			more->pop();
        }
    }
    return 0;
}
