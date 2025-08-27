/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 A thread pool that plays Tic-Tac-Toe all day.
 */
#ifndef _THREAD_POOL_H_
#define _THREAD_POOL_H_

#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include "task.h"

/**
 This class manages threads and plays Tic-Tac-Toe
 */
class threadPool{
private:
    int numberOfThreads;/**< threads are in our pool*/
    std::thread** myThreads;/**< Pointers to the threads*/
    bool* status;/**< Tracks which threads are awake and which are idle.*/
    bool done;/**< True when all work is done.*/
    std::condition_variable needsWork;/**< Threads wait for tasks.*/
    std::mutex queueLock;/**< Protect the task queue from race conditions.*/
    std::queue<task*>* toDO;/**< A list of tasks.*/
public:
    /**
     Create a new Thread Pool.
     */
    threadPool();
    /**
     Delete a thread pool from memory.
     */
    ~threadPool();
    /**
     Add a task to the todo list.
     @param t is the task to add.
     */
    void addWork(task* t);
    /**
     Start up the threads and run them.
     */
    void start();
private:
    /**
     Instructions for a worker thread to play tic-tac-toe
     @param statusID is the threads index into the array
     */
    void workerThread(int statusID);
    /**
     Stop the pool and exit all threads.
     */
    void stop();
};

#endif
