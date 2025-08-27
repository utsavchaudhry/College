/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 Implementation of search tasks.
 */
#include "searchTask.h"
#include <iostream>
#include <mutex>
searchTask::searchTask(std::vector<move> m){
    myMoves = m;
}

searchTask::~searchTask(){
    //Nothing to do
}

std::queue<task*>*  searchTask::runTask(){
	//Make a chess board
	board B;
	//Simulate the Game
	char lastMove = simulate(B);
	//If the game is over no new tasks
	if(B.gameOver()){
		printWinner(B);
		//No New Tasks
		return new std::queue<task*>();
	}
	//Else try every next move
	std::queue<task*>* nextRound = new std::queue<task*>();
	for(int i=0; i < B.getSize(); i++){
		if(B.get(i)=='E'){
			std::vector<move> nextMove = myMoves;
			move m;
			m.position = i;
			if(lastMove=='O'){
				m.piece = 'X';
			}else{
				m.piece ='O';
			}
			nextMove.push_back(m);
			nextRound->push(new searchTask(nextMove));
		}
	}
	return nextRound;
}

//Print the Winner
void searchTask::printWinner(board &B){
	static std::mutex ioLock;
	std::lock_guard<std::mutex> lk(ioLock);
	std::cout << "Winner: ";
	char w = B.winner();
	if(w=='E'){
		std::cout << "Tie Game";
	}else{
		std::cout << w;
	}
	std::cout << std::endl;
	printMoves(myMoves);
	std::cout << B << std::endl;
}

//Simulate a game
char searchTask::simulate(board &B){
	//Last Move made (default to O)
	char lastMove = 'O';
	//Make all the moves we are give
	for(int i=0; i < myMoves.size(); i++)
	{
		move m = myMoves[i];
		B.place(m.position, m.piece);
		lastMove = m.piece;
	}
	return lastMove;
}
