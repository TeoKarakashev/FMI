#include "Engine.h"
#include "../Entities/Hero/Human/Human.h"
#include "../Entities/Hero/Warrior/Warrior.h"
#include "../Entities/Hero/Wizard/Wizard.h"

void Engine::load() {
}

void Engine::createhero(Maze& maze) {
	std::cout << "Let's create our new hero and begin our journey" << std::endl;
	std::cout << "1 - Human: Health-50, Power-30, Mana-20" << std::endl;
	std::cout << "2 - Wizzard: Health-50, Power-10, Mana-40" << std::endl;
	std::cout << "3 - Warrior: Health-50, Power-40, Mana-10" << std::endl;


	int ch;
	std::cin >> ch;
		if (ch == 1) {
			Human human;
			maze.setHero(human);
		}
		else if (ch == 2) {
			Wizzard wizz;
			maze.setHero(wizz);
		}
		else if (ch == 3) {
			Warrior warrior;
			maze.setHero(warrior);
		}
}


void Engine::run() {

	Maze maze;

	createhero(maze);
	maze.generateLevel();
	maze.visualizeCurrentState();
	while (!_kbhit()) {
		Pair<int, int> heroCords = maze.getHeroCords();
		int ch = _getch();
		switch (ch)
		{
		case 'h':
			maze.toggleExtraData();
			break;
		case 'w':
			if (maze.moveHero(heroCords.c_first() - 1, heroCords.c_second())) {
				maze.visualizeCurrentState();

			}
			break;
		case 'a':
			if (maze.moveHero(heroCords.c_first(), heroCords.c_second() - 1)) {
				maze.visualizeCurrentState();
			}
			break;
		case 's':
			if (maze.moveHero(heroCords.c_first() + 1, heroCords.c_second())) {
				maze.visualizeCurrentState();
			}
			break;
		case 'd':
			if (maze.moveHero(heroCords.c_first(), heroCords.c_second() + 1)) {
				maze.visualizeCurrentState();
			}
			break;
		case 'q':
			if (maze.isOnTheExit()) {
				maze.generateLevel();
				maze.visualizeCurrentState();
			}
			break;
		case 'e':
			if (maze.isOnItem()) {
				maze.pickUpItem();
				maze.visualizeCurrentState();
			}
		default:
			break;
		}

	}

}
