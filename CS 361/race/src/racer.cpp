/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/25/24
 @section Description
 
 Thread safe racer behaviour that moves based on shared queue
 */

#include <vector>
#include <thread>
#include <random>
#include <chrono>
#include <thread>
#include <iostream>
#include "t_queue.h"
#include "racer.h"

// Function that simulates a racer in the race
void startRacer(bool& completed, ConQueue<int> &diceRolls, std::vector<std::thread::id> &winners) {
    
    if (completed) {
        return;
    }
    
    completed = false;
    int moves = 0;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> sleep_duration_gen(0, 2000);
    
    static std::mutex printLk;
    
    {std::lock_guard<std::mutex> lk(printLk);
        std::cout << "Thread " << std::this_thread::get_id() << " has left the gate." << std::endl;
    }
    
    while (moves < 20) {
        int dice_roll = diceRolls.waitToPop();
        moves += dice_roll;
        
        {std::lock_guard<std::mutex> lk(printLk);
            std::cout << "Thread " << std::this_thread::get_id() << " moved forward " << dice_roll << " spaces." << std::endl;
        }
        
        int sleep_duration = sleep_duration_gen(gen);
        std::this_thread::sleep_for(std::chrono::milliseconds(sleep_duration));
    }
    
    {std::lock_guard<std::mutex> lk(printLk);
        std::cout << "Thread " << std::this_thread::get_id() << " has crossed the finish line." << std::endl;
        winners.push_back(std::this_thread::get_id());
    }
    
    completed = true;
}
