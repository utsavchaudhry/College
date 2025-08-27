/**
 @mainpage CS361 - Race Homework
 @section Description
 
 Thread Race
 
 Make Commands:
 
     make
 
 will build the executable.
 
     make run
 
 will run the experiments.
 
     make clean

will clear out the compiled code.
 
    make doc
 
will build the doxygen files.
 
*/

/**
 @file
 @author Utsav Chaudhary <uc49@drexel.edu>
 @date 10/23/24
 @section Description
 
 The main program just creates threads for recer and game master
 */

#include <iostream>
#include <chrono>
#include <string>
#include <thread>
#include <vector>
//Custom Files
#include "racer.h"
#include "game_master.h"
#include "t_queue.h"

/**
 Run threads for game master and racers and track winners
 @param argc is the command line count, it must be 2
 @param argv is the command line arguments, argv[1] must be a positive integer
 @return always 0 no matter what
 */
int main(int argc, char** argv)
{
	if (argc!=2) {
        std::cout << "Usage: "
            << argv[0] << " [Number of Racers]"
        << std::endl;
        std::cout << "Number of Racers is a positive integer"
        << std::endl;
        return 0;
    }
    
    std::string arg(argv[1]);
    int option=0;
    try {
        option = stoi(arg);
        if (option < 1) {
            throw std::runtime_error("Value must be a positive integer");
        }
    } catch (std::exception &e) {
        std::cout << "Not a valid option." << std::endl;
        std::cout << "Error: " << e.what() << std::endl;
        std::cout << "Usage: "
            << argv[0] << " [Number of Racers]"
        << std::endl;
        std::cout << "Number of Racers is a positive integer"
        << std::endl;
        return 0;
    }
    
    ConQueue<int> diceRolls;
    
    bool all_racer_completed = false;
    std::thread gameMaster(beginRace, std::ref(all_racer_completed), std::ref(diceRolls));
    
    std::vector<std::shared_ptr<bool>> racer_completed_flags(option);
    std::vector<std::thread> racers;
    std::vector<std::thread::id> winners(0);
    for (int i = 0; i < option; i++) {
        racer_completed_flags[i] = std::make_shared<bool>(false);
        racers.emplace_back(startRacer, std::ref(*racer_completed_flags[i]), std::ref(diceRolls), std::ref(winners));
    }
    
    while (!all_racer_completed) {
        for (int i = 0; i < option; i++) {
            if (!(*racer_completed_flags[i])) {
                break;
            }
            
            if (i == (option - 1)) {
                all_racer_completed = true;
            }
        }
    }
    
    for (int i = 0; i < option; i++) {
        racers[i].join();
    }
    
    gameMaster.join();
    
    for (int i = 0; i < option; i++) {
        std::cout << (i+1) << ": " << winners[i] << std::endl;
    }
	
	return 0;
}

