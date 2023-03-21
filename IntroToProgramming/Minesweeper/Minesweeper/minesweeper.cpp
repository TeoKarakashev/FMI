#include<iostream>
#include<cmath>
#include <time.h> 
#include<cstring>

void showOriginalBoard(int** matrix, unsigned size) {
	for (int row = 0; row < size; row++) {
		for (int col = 0; col < size; col++) {
			std::cout << "[ " << matrix[row][col] << " ] ";
		}
		std::cout << std::endl;
	}
}

char toChar(int number) {
	return '0' + number;
}

void displayCurrentBoard(char** matrix, unsigned size, bool validInput) {
	system("cls");
	for (int row = 0; row < size; row++) {
		for (int col = 0; col < size; col++) {
			std::cout << "[ " << matrix[row][col] << " ] ";
		}
		std::cout << std::endl;
	}
	if (!validInput) {
		std::cout << "Please enter a valid input" << std::endl;
	}
}

void generateMines(int** matrix, unsigned size, unsigned numberOfmines) {
	srand(time(NULL));
	while (numberOfmines > 0) {
		int row = rand() % size;
		int col = rand() % size;
		if (matrix[row][col] != 9) {
			matrix[row][col] = 9;
			numberOfmines--;
		}
	}
}

bool isValid(int row, int col, unsigned size)
{
	return (row >= 0) && (row < size) &&
		(col >= 0) && (col < size);
}

bool isMine(int** matrix, int row, int col) {
	return matrix[row][col] == 9;
}

bool cellContains(char** board, int row, int col, char character) {
	return board[row][col] == character;
}

bool isNotOpened(char** board, int row, int col) {
	return cellContains(board, row, col, '-');
}

int countAdjacentMines(int** matrix, int row, int col, unsigned size) {
	int count = 0;
	if (isValid(row - 1, col - 1, size)) {
		if (isMine(matrix, row - 1, col - 1)) count++;
	}
	if (isValid(row - 1, col, size)) {
		if (isMine(matrix, row - 1, col)) count++;
	}
	if (isValid(row - 1, col + 1, size)) {
		if (isMine(matrix, row - 1, col + 1)) count++;
	}
	if (isValid(row, col - 1, size)) {
		if (isMine(matrix, row, col - 1)) count++;
	}
	if (isValid(row, col + 1, size)) {
		if (isMine(matrix, row, col + 1)) count++;
	}
	if (isValid(row + 1, col - 1, size)) {
		if (isMine(matrix, row + 1, col - 1)) count++;
	}
	if (isValid(row + 1, col, size)) {
		if (isMine(matrix, row + 1, col)) count++;
	}
	if (isValid(row + 1, col + 1, size)) {
		if (isMine(matrix, row + 1, col + 1)) count++;
	}
	return count;
}

void setPowerOfAllCells(int** matrix, unsigned size) {
	for (int row = 0; row < size; row++) {
		for (int col = 0; col < size; col++) {
			int countOfNeighbourMines = 0;
			if (matrix[row][col] != 9) {
				countOfNeighbourMines = countAdjacentMines(matrix, row, col, size);
				matrix[row][col] = countOfNeighbourMines;
			}
		}
	}
}

void openAdjacentCell(int** matrix, char** playingBoard, int row, int col, unsigned size) {
	if (isNotOpened(playingBoard, row, col)) {
		playingBoard[row][col] = toChar(matrix[row][col]);

	}
}

void openAllAdjacentCells(int** matrix, char** playingBoard, int row, int col, unsigned size, unsigned& cellsToOpen) {
	if (isValid(row - 1, col - 1, size) && isNotOpened(playingBoard, row - 1, col - 1)) {
		openAdjacentCell(matrix, playingBoard, row - 1, col - 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row - 1, col - 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row - 1, col - 1, size, cellsToOpen);
		}
	}
	if (isValid(row - 1, col, size) && isNotOpened(playingBoard, row - 1, col)) {
		openAdjacentCell(matrix, playingBoard, row - 1, col, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row - 1, col, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row - 1, col, size, cellsToOpen);
		}
	}
	if (isValid(row - 1, col + 1, size) && isNotOpened(playingBoard, row - 1, col + 1)) {
		openAdjacentCell(matrix, playingBoard, row - 1, col + 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row - 1, col + 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row - 1, col + 1, size, cellsToOpen);
		}
	}
	if (isValid(row, col - 1, size) && isNotOpened(playingBoard, row, col - 1)) {
		openAdjacentCell(matrix, playingBoard, row, col - 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row, col - 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row, col - 1, size, cellsToOpen);
		}
	}
	if (isValid(row, col + 1, size) && isNotOpened(playingBoard, row, col + 1)) {
		openAdjacentCell(matrix, playingBoard, row, col + 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row, col + 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row, col + 1, size, cellsToOpen);
		}
	}
	if (isValid(row + 1, col - 1, size) && isNotOpened(playingBoard, row + 1, col - 1)) {
		openAdjacentCell(matrix, playingBoard, row + 1, col - 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row + 1, col - 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row + 1, col - 1, size, cellsToOpen);
		}
	}
	if (isValid(row + 1, col, size) && isNotOpened(playingBoard, row + 1, col)) {
		openAdjacentCell(matrix, playingBoard, row + 1, col, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row + 1, col, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row + 1, col, size, cellsToOpen);
		}
	}
	if (isValid(row + 1, col + 1, size) && isNotOpened(playingBoard, row + 1, col + 1)) {
		openAdjacentCell(matrix, playingBoard, row + 1, col + 1, size);
		cellsToOpen--;
		if (cellContains(playingBoard, row + 1, col + 1, '0')) {
			openAllAdjacentCells(matrix, playingBoard, row + 1, col + 1, size, cellsToOpen);
		}
	}
}

void initBoard(int** matrix, unsigned size, unsigned numberOfMines) {
	for (int i = 0; i < size; i++) {
		matrix[i] = new int[size];
	}
	generateMines(matrix, size, numberOfMines);
	setPowerOfAllCells(matrix, size);
}

void initBoardMetrics(unsigned& sizeOfBoard, unsigned& numberOfMines) {
	while (sizeOfBoard < 3 || sizeOfBoard > 10) {
		std::cout << "Enter board size (3<=N<=10): ";
		std::cin >> sizeOfBoard;
	}
	while (numberOfMines <1 || numberOfMines >(3 * sizeOfBoard)) {
		std::cout << "Enter number of mines (1<=K<=3N): ";
		std::cin >> numberOfMines;
	}

}

void deleteBoard(int** matrix, unsigned sizeOfMatrix) {
	for (int i = 0; i < sizeOfMatrix; ++i) {
		delete[] matrix[i];
	}
	delete[] matrix;
}

void fillAllCellsOfPlayingBoard(char** matrix, unsigned size) {
	for (int row = 0; row < size; row++) {
		matrix[row] = new char[size];
		for (int col = 0; col < size; col++) {
			matrix[row][col] = '-';
		}
	}
}

void deletePlayingBoard(char** board, unsigned size) {
	for (int i = 0; i < size; ++i) {
		delete[] board[i];
	}
	delete[] board;
}

void openCell(int** board, char** playingBoard, int row, int col, bool& openedMine, unsigned size, unsigned& cellsToOpen) {
	if (isMine(board, row, col)) {
		openedMine = true;
	}
	else if (isNotOpened(playingBoard, row, col) && !cellContains(playingBoard, row, col, '*')) {
		playingBoard[row][col] = toChar(board[row][col]);
		cellsToOpen--;
	}

	if (cellContains(playingBoard, row, col, '0')) {
		openAllAdjacentCells(board, playingBoard, row, col, size, cellsToOpen);
	}
}

void markCell(char** playingBoard, int row, int col, unsigned& marksCount) {
	if (cellContains(playingBoard, row, col, '-') && marksCount > 0) {
		playingBoard[row][col] = '*';
		marksCount--;
	}
}

void unMarkCell(char** playingBoard, int row, int col, unsigned& marksCount) {
	if (cellContains(playingBoard, row, col, '*')) {
		playingBoard[row][col] = '-';
		marksCount++;
	}
}

int compare(char a[], const char b[]) {
	for (int i = 0; b[i] != '\0'; i++) {
		if (a[i] != b[i])
			return 0;
	}
	return 1;
}

void initCommand(char* command, int& row, int& col) {
	std::cin.getline(command, 8, ' ');
	std::cin >> row;
	std::cin >> col;

}

void printMessages(unsigned marksCount) {

	std::cout << "Marks left: " << marksCount << std::endl;
	std::cout << "Possible commands: " << std::endl;
	std::cout << "open [row][col]" << std::endl;
	std::cout << "mark [row][col]" << std::endl;
	std::cout << "unmark [row][col]" << std::endl;
}

void play(int** board, unsigned size, int minesCount) {
	char** playingBoard = new char* [size];
	fillAllCellsOfPlayingBoard(playingBoard, size);
	bool finished = false;
	bool openedMine = false;
	bool isValidInput = true;
	char command[8];
	int row, col;
	unsigned marksCount = minesCount;
	unsigned cellsToOpen = pow(size, 2) - minesCount;

	while (!finished && cellsToOpen > 0) {

		displayCurrentBoard(playingBoard, size, isValidInput);

		printMessages(marksCount);

		initCommand(command, row, col);

		if (isValid(row, col, size)) {
			if (compare(command, "\nopen")) {
				openCell(board, playingBoard, row, col, openedMine, size, cellsToOpen);
				isValidInput = true;
			}
			else if (compare(command, "\nmark")) {
				markCell(playingBoard, row, col, marksCount);
				isValidInput = true;
			}
			else if (compare(command, "\nunmark")) {
				unMarkCell(playingBoard, row, col, marksCount);
				isValidInput = true;
			}
			else {
				isValidInput = false;
			}
		}
		else {
			isValidInput = false;
		}

		if (openedMine) {
			showOriginalBoard(board, size);
			std::cout << "Oops, you opened a mine and lost" << std::endl;
			finished = true;
		}
		else if (!cellsToOpen) {
			displayCurrentBoard(playingBoard, size, true);
			std::cout << "Congratulations! You won!" << std::endl;
			finished = true;
		}
	}
	deletePlayingBoard(playingBoard, size);
}

int main() {

	unsigned sizeOfBoard;
	unsigned numberOfMines;

	initBoardMetrics(sizeOfBoard, numberOfMines);

	int** board = new int* [sizeOfBoard];

	initBoard(board, sizeOfBoard, numberOfMines);
	play(board, sizeOfBoard, numberOfMines);
	deleteBoard(board, sizeOfBoard);

}