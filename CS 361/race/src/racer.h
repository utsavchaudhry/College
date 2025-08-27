#include <vector>
#include <thread>
#include "t_queue.h"

// Function that simulates a racer in the race
void startRacer(bool& completed, ConQueue<int> &diceRolls, std::vector<std::thread::id> &winners);
