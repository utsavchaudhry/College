/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 Implementation of printing move list.
 */
#include "move.h"

void printMoves(std::vector<move> M){
	std::cout << "Total Moves: "
		<< M.size()
		<< std::endl;
	for(int i=0; i < M.size(); i++)
	{
		std::cout
			<< "Turn "
			<< (i+1) << ": "
			<< M[i].position
			<< " "
			<< M[i].piece
			<< std::endl;
	}
}
