/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 Implementation of the board class.
 */

#include "board.h"

//Constructor
board::board(){
	spaces = new char[size];
	for(int i=0; i < size; i++){
		spaces[i] = 'E';
	}
}

//Destructor
board::~board(){
	delete spaces;
}

//Place a new move
bool board::place(int loc, char sym){
	//Below Board
	if(loc < 0){return false;}
	//Above Board
	if(loc >= size){return false;}
	//Space Taken
	if(spaces[loc]!='E'){return false;}
	//Make Move
	spaces[loc]=sym;
	return true;
}

//Check if game over
bool board::gameOver() const{
	//Someone Won
	if(winner()!='E'){
		return true;
	}
	//Tie No more Moves
	for(int i=0; i < size; i++){
		if(spaces[i]=='E'){
			return false;//Moves available
		}
	}
	//Tie
	return true;
}

//Look for winner
char board::winner() const{
	//Check Rows
	for(int i=0; i < 3; i++){
		if(
			spaces[i*3]==spaces[i*3+1]
			&&
			spaces[i*3+1]==spaces[i*3+2]
			&&
			spaces[i*3+1]!='E')
		{
			return spaces[i*3];
		}
	}
	//Check Cols
	for(int i=0; i < 3; i++){
		if(
			spaces[i]==spaces[i+3]
			&&
			spaces[i+3]==spaces[i+6]
			&&
			spaces[i]!='E')
		{
			return spaces[i];
		}
	}
	//Diagonal to the left
	if(
		spaces[0]==spaces[4]
		&&
		spaces[4]==spaces[8]
		&&
		spaces[0]!='E')
	{
		return spaces[0];
	}
	//Diagonal to the right
	if(
		spaces[2]==spaces[4]
		&&
		spaces[4]==spaces[6]
		&&
		spaces[2]!='E')
	{
		return spaces[2];
	}
	return 'E';//none
}

//Get a location
char board::get(int pos) const{
	return spaces[pos];
}
//Get Size
int board::getSize() const{
	return size;
}

//Overload the print operator
//This isn't a friend since
//we can do everything with methods.
std::ostream& operator<<(std::ostream& os, const board& b){
	os << "Chess Board" << std::endl;
	for(int i=0; i < b.getSize(); i++){
		os << "| " << b.get(i) << " ";
		if(i%3==2){
			os << "|" << std::endl;
		}
	}
	return os;
}
