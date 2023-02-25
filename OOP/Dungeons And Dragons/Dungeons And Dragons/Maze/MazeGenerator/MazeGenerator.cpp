#include "MazeGenerator.h"

void MazeGenerator::ResetGrid()
{
	// Fills the grid with walls ('#' characters).
	for (int i = 0; i < gridWidth * gridHeight; ++i)
	{
		grid[i] = '#';
	}
}
int MazeGenerator::XYToIndex(int x, int y)
{
	// Converts the two-dimensional index pair (x,y) into a
	// single-dimensional index. The result is y * ROW_WIDTH + x.
	return y * gridWidth + x;
}
int MazeGenerator::IsInBounds(int x, int y)
{
	// Returns "true" if x and y are both in-bounds.
	if (x < 0 || x >= gridWidth) return false;
	if (y < 0 || y >= gridHeight) return false;
	return true;
}

void MazeGenerator::Visit(int x, int y)
{
	// Starting at the given index, recursively visits every direction in a
	// randomized order.
	// Set my current location to be an empty passage.
	grid[XYToIndex(x, y)] = ' ';
	// Create an local array containing the 4 directions and shuffle their order.
	int dirs[4];
	dirs[0] = NORTH;
	dirs[1] = EAST;
	dirs[2] = SOUTH;
	dirs[3] = WEST;
	for (int i = 0; i < 4; ++i)
	{
		int r = rand() & 3;
		int temp = dirs[r];
		dirs[r] = dirs[i];
		dirs[i] = temp;
	}
	// Loop through every direction and attempt to Visit that direction.
	for (int i = 0; i < 4; ++i)
	{
		// dx,dy are offsets from current location. Set them based
		// on the next direction I wish to try.
		int dx = 0, dy = 0;
		switch (dirs[i])
		{
		case NORTH: dy = -1; break;
		case SOUTH: dy = 1; break;
		case EAST: dx = 1; break;
		case WEST: dx = -1; break;
		}
		// Find the (x,y) coordinates of the grid cell 2 spots
		// away in the given direction.
		int x2 = x + (dx << 1);
		int y2 = y + (dy << 1);
		if (IsInBounds(x2, y2))
		{
			if (grid[XYToIndex(x2, y2)] == '#')
			{
				// (x2,y2) has not been visited yet... knock down the
				// wall between my current position and that position
				grid[XYToIndex(x2 - dx, y2 - dy)] = ' ';
				// Recursively Visit (x2,y2)
				Visit(x2, y2);
			}
		}
	}
}


char** MazeGenerator::gridToMatrix() {

	char** matrix = new char*[gridHeight];
	for (int y = 0; y < gridHeight; ++y) {
		matrix[y] = new char[gridWidth];
	}
	for (int y = 0; y < gridHeight; ++y) {
		for (int x = 0; x < gridWidth; ++x) {
			matrix[y][x] = grid[XYToIndex(x, y)];
		}
	}
	return matrix;
}

void MazeGenerator::free() {
	delete[] grid;
}

void MazeGenerator::copyFrom(const MazeGenerator& other) {
	grid = new char[strlen(other.grid) + 1];
	strcpy(grid, other.grid);
	gridWidth = other.gridWidth;
	gridHeight = other.gridHeight;
}

MazeGenerator::MazeGenerator() {
	grid = nullptr;
	gridHeight = 0;
	gridWidth = 0;
}

MazeGenerator::MazeGenerator(const MazeGenerator& other) {
	copyFrom(other);
}

MazeGenerator& MazeGenerator::operator=(const MazeGenerator& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

MazeGenerator::~MazeGenerator() {
	free();
}

char** MazeGenerator::generateMaze(int width, int height) {
	this->gridWidth = width;
	this->gridHeight = height;
	grid = new char[gridWidth * gridHeight];
	srand(time(0));
	ResetGrid();
	Visit(1, 1);
	return gridToMatrix();

}


