/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/25/24
 @section Description
 
 Random and periodic dice roll generator
 */

#include <random>
#include <chrono>
#include <thread>
#include <iostream>
#include "game_master.h"
#include "t_queue.h"

// Function that simulates the game master generating dice rolls
void beginRace(bool& completed, ConQueue<int> &diceRolls) {
    
    if (completed) {
        return;
    }
    
    completed = false;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> k_gen(0, 5);
    std::uniform_int_distribution<> dice_roll_gen(1, 6);
    std::uniform_int_distribution<int> sleep_duration_gen(0, 2000);
    
    while (!completed) {
        int k = k_gen(gen);
        for (int i = 0; i < k; i++) {
            int dice_roll = dice_roll_gen(gen);
            diceRolls.push(dice_roll);
        }
        
        int sleep_duration = sleep_duration_gen(gen);
        std::this_thread::sleep_for(std::chrono::milliseconds(sleep_duration));
    }
}
