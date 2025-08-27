/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 This class represents a Tic-Tac-Toe Board.
 */
#ifndef _BOARD_H_
#define _BOARD_H_
#include <iostream>

/**
 A Tic-Tac-Toe Board with some moves made.
 */
class board{
private:
	char* spaces;/**< The spaces of the board.*/
	int size = 9;/**< Number of spaces.*/
public:
	/**
     Create an empty board with no moves.
    */
	board();
	/**
     Delete a board from memory.
    */
	~board();
    /**
     Place Piece true if success, false if illegal move
     @param loc is the position to place the peice
     @param sym is the X/O to place
     @return true is sucessful
    */
	bool place(int loc, char sym);
    /**
     Check if the game is over
     @return true when game is over
    */
	bool gameOver() const;
	/**
     Determine Winner
     @return X or O unless tie then E
    */
	char winner() const;
	/**
     Get move at position
     @param pos is the position to check
     @return the move in that space
    */
	char get(int pos) const;
	/**
     Get the size of the board
     @return the size of the board
     */
	int getSize() const;
};
/**
 Print out the board.
 @param os is the stream to print to
 @param b is the board to print
 @return the updated stream
 */
std::ostream& operator<<(std::ostream& os, const board& b);

#endif
