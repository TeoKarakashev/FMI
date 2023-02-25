#pragma once
// Algorithm taken from https://www.cefns.nau.edu/~pek7/CS200/Project%209.pdf
// Refactoring was required

#include <iostream>
#pragma warning(disable:4996)
#define NORTH 0
#define EAST 1
#define SOUTH 2
#define WEST 3
class MazeGenerator {
private:


	int gridWidth;
	int gridHeight; 
	char* grid;


	void ResetGrid();
	int XYToIndex(int x, int y);
	int IsInBounds(int x, int y);
	void Visit(int x, int y);
	char** gridToMatrix();

	void free();
	void copyFrom(const MazeGenerator& other);

public:

	MazeGenerator();
	MazeGenerator(const MazeGenerator& other);
	MazeGenerator& operator=(const MazeGenerator& other);
	~MazeGenerator();

	char** generateMaze(int width, int height);
};
