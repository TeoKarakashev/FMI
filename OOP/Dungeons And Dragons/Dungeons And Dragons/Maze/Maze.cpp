#include "Maze.h"

void Maze::free() {
	for (int i = 0; i < height; i++) {
		delete grid[i];
	}
	delete[] grid;
}

void Maze::copyFrom(const Maze& other) {
	height = other.height;
	width = other.width;
	grid = new char* [height];
	for (int i = 0; i < height; i++) {
		grid[i] = new char[width];
	}
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			grid[x][y] = other.grid[x][y];
		}
	}
}

bool Maze::isValidMove(int x, int y)
{
	return grid[x][y] != '#';
}

void Maze::generateMonsters() {
	int count = 0;
	while (count <= currentLevel) {
		int widthCord = (rand() % (width - 2)) + 1;
		int heightCord = (rand() % (height - 2)) + 1;
		if (grid[heightCord][widthCord] == ' ') {
			Monster monster;
			monster.setHealth(50 + (10 * (currentLevel - 1)));
			monster.setCurrentHealth(monster.getHealth());
			monster.setMana(25 + (10 * (currentLevel - 1)));
			monster.setPower(25 + (10 * (currentLevel - 1)));
			monster.setShield(15 + (5 * (currentLevel - 1)));
			monsters.add(monster);
			grid[heightCord][widthCord] = 'M';
			Pair<int, int> cords;
			cords.set_pair(heightCord, widthCord);
			monstersCoordiantes.add(cords);
			count++;
		}
	}
}

void Maze::generateTreasures() {
	items.clear();
	itemsCoordinates.clear();
	itemTypes.clear();
	int count = 0;
	while (count <= currentLevel) {
		int widthCord = (rand() % (width - 2)) + 1;
		int heightCord = (rand() % (height - 2)) + 1;
		if (grid[heightCord][widthCord] == ' ') {
			grid[heightCord][widthCord] = 'T';
			Pair<int, int> cords;
			cords.set_pair(heightCord, widthCord);
			generateRandomItem();
			itemsCoordinates.add(cords);
			count++;
		}
	}
}

void Maze::levelUpHero() {
	system("cls");
	int max = 30;
	std::cout << "You get 30 points for leveling up, use them wisely:" << std::endl;
	std::cout << "Eneter power points: ";
	int power;
	std::cin >> power;
	std::cout << "Eneter mana points: ";
	int mana;
	std::cin >> mana;
	std::cout << "Eneter health points: ";
	int health;
	std::cin >> health;
	if (power + mana + health != 30) {
		std::cout << "exactly 30 points for your stats! " << std::endl;
		while (power + mana + health != 30){
			std::cout << "Eneter power points: ";
			std::cin >> power;
			std::cout << "Eneter mana points: ";
			std::cin >> mana;
			std::cout << "Eneter health points: ";
			std::cin >> health;
		}
	}
	heroEntity.getHero()->levelUp(power, mana, health);

}

void Maze::generateRandomItem() {
	int itemType = (rand() % 3) + 1;
	int itemIndex = (rand() % 5);
	itemTypes.add(itemType);
	int percentIncrease = 25 + (5 * (currentLevel - 1));
	if (itemType == ARMOUR_INDEX) {
		String name = armourNames[itemIndex];
		items.addItem(Armour(name, percentIncrease));
	}
	else if (itemType == SPELL_INDEX) {
		String name = spellNames[itemIndex];
		items.addItem(Spell(name, percentIncrease));
	}
	else {
		String name = weaponNames[itemIndex];
		items.addItem(Weapon(name, percentIncrease));
	}

}

Maze::Maze() {
	height = 0;
	width = 0;
	grid = nullptr;
	previousStateOfCurrentHeroPosition = ' ';
	currentLevel = 0;
	visualizeMazeData = false;
}

//Maze::Maze(const Maze& other) {
//	copyFrom(other);
//}

//Maze& Maze::operator=(const Maze& other) {
//	if (this != &other) {
//		free();
//		copyFrom(other);
//	}
//	return *this;
//}

Maze::~Maze() {
	free();
}

void Maze::generateMaze(unsigned width, unsigned height) {
	srand(time(0));
	this->width = width;
	this->height = height;
	MazeGenerator generator;
	grid = generator.generateMaze(width, height);
	grid[1][1] = 'H';
	previousStateOfCurrentHeroPosition = ' ';
	heroCoordinates.set_pair(1, 1);
	grid[height - 2][width - 2] = 'E';

	generateTreasures();
	generateMonsters();
}

int Maze::findCurrentTresure() const {
	for (int i = 0; i < itemsCoordinates.Size(); i++) {
		if ((heroCoordinates.c_first() == itemsCoordinates[i].c_first()) && (heroCoordinates.c_second() == itemsCoordinates[i].c_second())) {
			return i;
		}
	}
	return -1;
}

int Maze::findCurrentMonster() const
{
	for (int i = 0; i < monstersCoordiantes.Size(); i++) {
		if ((heroCoordinates.c_first() == monstersCoordiantes[i].c_first()) && (heroCoordinates.c_second() == monstersCoordiantes[i].c_second())) {
			return i;
		}
	}
	return -1;
	return 0;
}

void Maze::visualizeExtraInfo() const {
	std::cout << std::endl;
	std::cout << "Hero max health:" << heroEntity.getHero()->getHealth() << std::endl;
	std::cout << "Hero current health:" << heroEntity.getHero()->getCurrentHealth() << std::endl;
	std::cout << "Hero mana:" << heroEntity.getHero()->getMana() << std::endl;
	std::cout << "Hero weapon power:" << heroEntity.getHero()->getWeapon().getPercentIncrease() << std::endl;
	std::cout << "Hero spell power:" << heroEntity.getHero()->getSpell().getPercentIncrease() << std::endl;
	try {
		std::cout << "Hero armour shield:" << heroEntity.getHero()->getArmour().getData().getPercentIncrease()<<"%" << std::endl;
	}
	catch (std::logic_error) {
		std::cout << "Hero armour shield : 0%" << std::endl;
	}
}

bool Maze::isOnItem() const
{
	return previousStateOfCurrentHeroPosition == 'T';
}

void Maze::setHero(const Hero& hero) {
	heroEntity.addHero(hero);
}

bool Maze::isOnTheExit() const {
	return previousStateOfCurrentHeroPosition == 'E';
}

void Maze::generateLevel() {
	currentLevel++;
	if (currentLevel > 1) {
	levelUpHero();
	}
	if (currentLevel == 1) {
		generateMaze(11, 11);
	}
	else if (currentLevel == 2) {
		generateMaze(15, 11);
	}
	else {

		if ((width += WIDTH_INCREASE) % 2 == 0) {
			width++;
		};
		if ((height += HEIGHT_INCREASE) % 2 == 0) {
			height++;
		};
		generateMaze(width, height);
	}
}

const Pair<int, int> Maze::getHeroCords() const {

	return heroCoordinates;
}

bool Maze::moveHero(int x, int y) {
	if (isValidMove(x, y)) {
		healthRegen();
		grid[heroCoordinates.c_first()][heroCoordinates.c_second()] = previousStateOfCurrentHeroPosition;
		heroCoordinates.set_pair(x, y);
		previousStateOfCurrentHeroPosition = grid[x][y];
		grid[x][y] = 'H';
		return true;
	}
	return false;
}

void Maze::visualizeCurrentState() const {
	int currentTreasueIndex = -1;
	int currentMonsterIndex = -1;
	visualizeMaze();
	switch (previousStateOfCurrentHeroPosition) {
	case 'E':
		std::cout << "Congratulations! You made your way to the end of the current level! Do you want to procced? Press q";
		if (visualizeMazeData) {
			visualizeExtraInfo();
		}
		break;
	case 'M':
		currentMonsterIndex = findCurrentMonster();
		if (heroEntity.getHero()->getCurrentHealth() < monsters[currentMonsterIndex].getHealth()) {
			std::cout << "Ooh you died!";
			exit(1);
		}
		/*Monster monster = monsters[currentMonsterIndex];
		beginBattle(monsters[currentMonsterIndex]); */
		std::cout << "Monster health:"<<monsters[currentMonsterIndex].getHealth()<<std::endl;
		std::cout << "Monster power:"<<monsters[currentMonsterIndex].getPower() << std::endl;
		std::cout << "Monster Mana:"<<monsters[currentMonsterIndex].getMana() << std::endl;
		std::cout << "Monster Shiled:"<<monsters[currentMonsterIndex].getShield()<<"%" << std::endl;

		if (visualizeMazeData) {
			visualizeExtraInfo();
		}
		break;
	case 'T':
		currentTreasueIndex = findCurrentTresure();
		std::cout << "Ooh, you found a treasure" << std::endl;
		std::cout << "Name: " << items.getAt(currentTreasueIndex)->getName() << std::endl;
		std::cout << "Percentage bonus: " << items.getAt(currentTreasueIndex)->getPercentIncrease() << std::endl;
		if (itemTypes[currentTreasueIndex] == ARMOUR_INDEX) {
			try {
				std::cout << "Your current Armour is: " << heroEntity.getHero()->getArmour().getData().getName() << std::endl;
				std::cout << "Current percentage bonus: " << heroEntity.getHero()->getArmour().getData().getPercentIncrease() << std::endl;
			}
			catch (std::logic_error) {
				std::cout << " You don't have an armour currently" << std::endl;
			}
		}
		else if (itemTypes[currentTreasueIndex] == WEAPON_INDEX) {
			std::cout << "Your current weapon is: " << heroEntity.getHero()->getWeapon().getName() << std::endl;
			std::cout << "Current percentage bonus: " << heroEntity.getHero()->getWeapon().getPercentIncrease() << std::endl;
		}
		else {
			std::cout << "Your current spell is: " << heroEntity.getHero()->getSpell().getName() << std::endl;
			std::cout << "Current percentage bonus: " << heroEntity.getHero()->getSpell().getPercentIncrease() << std::endl;
		}
		std::cout << "Press e to pick it up" << std::endl;
		if (visualizeMazeData) {
			visualizeExtraInfo();
		}
		break;
	default:
		if (visualizeMazeData) {
			visualizeExtraInfo();
		}
		break;
	}
}

bool Maze::pickUpItem() {
	int currentTreasueIndex = findCurrentTresure();
	if (itemTypes[currentTreasueIndex] == ARMOUR_INDEX) {
		Armour a(items.getAt(currentTreasueIndex)->getName(), items.getAt(currentTreasueIndex)->getPercentIncrease());
		try {
			items.setAt(currentTreasueIndex, heroEntity.getHero()->getArmour().getData().getName(),
				heroEntity.getHero()->getArmour().getData().getPercentIncrease());
			heroEntity.getHero()->setArmour(a);
		}
		catch (std::logic_error) {
			heroEntity.getHero()->setArmour(a);
			previousStateOfCurrentHeroPosition = ' ';
		}
	}
	else if (itemTypes[currentTreasueIndex] == WEAPON_INDEX) {
		Weapon w(items.getAt(currentTreasueIndex)->getName(), items.getAt(currentTreasueIndex)->getPercentIncrease());
		items.setAt(currentTreasueIndex, heroEntity.getHero()->getWeapon().getName(),
			heroEntity.getHero()->getWeapon().getPercentIncrease());
		heroEntity.getHero()->setWeapon(w);

	}
	else {
		Spell s(items.getAt(currentTreasueIndex)->getName(), items.getAt(currentTreasueIndex)->getPercentIncrease());
		items.setAt(currentTreasueIndex, heroEntity.getHero()->getSpell().getName(),
			heroEntity.getHero()->getSpell().getPercentIncrease());
		heroEntity.getHero()->setSpell(s);
	}
	return true;
}

void Maze::beginBattle(Monster& monster) const{
	Battle battle;
	battle.fight(heroEntity.getHero(), monster);
}

void Maze::toggleExtraData() {
	if (visualizeMazeData) {
		visualizeMazeData = false;
	}
	else {

	visualizeMazeData = true;
	}
}

void Maze::healthRegen() {
	heroEntity.getHero()->healthRegen();
}

void Maze::visualizeMaze() const {
	system("cls");
	for (int i = 0; i < height; ++i)
	{
		for (int j = 0; j < width; ++j)
		{
			std::cout << grid[i][j];
		}
		std::cout << std::endl;
	}
}
