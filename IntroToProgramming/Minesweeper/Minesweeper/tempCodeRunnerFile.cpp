
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