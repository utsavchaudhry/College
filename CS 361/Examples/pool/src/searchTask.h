/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 A Task for the board. Simulate the board and see who won.
 */
#ifndef _SEARCH_TASK_H_
#define _SEARCH_TASK_H_
#include <vector>
#include "task.h"
#include "move.h"
#include "board.h"

/**
 A task that is a list of game moves.
 Simulate the same and see who won or
 make more moves.
 */
class searchTask: public task{
private:
    std::vector<move> myMoves;/**< Moves to play in the game. */
public:
    /**
     Create a new task using the moves given.
     @param m contains the moves to use.
     */
    searchTask(std::vector<move> m);
    /**
     Destory a task.
     */
    ~searchTask();
    /**
     Run the task (in a thread)
     @return Any new tasks created by this run.
     */
    std::queue<task*>* runTask();
private:
    /**
     Print out who won the game.
     @param B is the board with the winner decided.
     */
    void printWinner(board &B);
	/**
     Simulate game and return last move
     @param B is the board to simulate on
     @return is the last move made (who went last)
     */
    char simulate(board &B);
};

#endif

