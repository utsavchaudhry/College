/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 A move is a action (X,0) and the place to put it in.
 */

#ifndef _MOVE_H_
#define _MOVE_H_
#include <vector>
#include <iostream>

/**
 Store a single move.
*/
struct move{
	int position;/**< index is where the piece goes **/
	char piece;/**< char is the move (X or O)*/
};

/**
 Print a list of moves
 @param M is a vector full of moves
*/
void printMoves(std::vector<move> M);

#endif
