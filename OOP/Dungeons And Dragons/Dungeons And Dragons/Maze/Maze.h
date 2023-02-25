#pragma once
#include "MazeGenerator/MazeGenerator.h"
#include "../Entities/Hero/HeroContainer/HeroContainer.h"
#include "../Helpers/Pair/Pair.hpp"
#include "../Helpers/ArrayList/ArrayList.hpp"
#include "../Entities/Item/ItemCollection/ItemCollection.h"
#include "../Entities/Monster/Monster.h"
#include "../Battle/Battle.h"
#define WIDTH_INCREASE 5
#define HEIGHT_INCREASE 3
#define ARMOUR_INDEX 1
#define WEAPON_INDEX 2
#define SPELL_INDEX 3

static const char* armourNames[] =
{ "Vest Of Smoldering Hells", "Warrior Mithril Tunic", "Skeletal Chestplate Of Dark Hope", "Fall Of Corruption", "Tunic Of Immortal Comrades"};

static const char* weaponNames[] =
{ "Sleep walker", "Shadow feather", "Hubris", "Starlight", "Guardian Skull" };

static const char* spellNames[] =
{ "Nether Strike", "Frost Assault", "Shooting Star", "Blast Of The Total Eclipse", "Drain Soul" };

class Maze {

	unsigned height;
	unsigned width;
	unsigned currentLevel;
	char** grid;
	char previousStateOfCurrentHeroPosition;
	bool visualizeMazeData;

	HeroContainer heroEntity;
	Pair<int, int> heroCoordinates;

	ItemCollection items;
	ArrayList<Pair<int, int>> itemsCoordinates;
	ArrayList<int> itemTypes;

	ArrayList<Pair<int, int>> monstersCoordiantes;
	ArrayList<Monster> monsters;

	void free();
	void copyFrom(const Maze& other);
	bool isValidMove(int x, int y);
	void generateMonsters();
	void generateTreasures();
	void levelUpHero();
	void generateRandomItem();
	void visualizeMaze() const;
	void generateMaze(unsigned height, unsigned width);
	int findCurrentTresure() const;
	int findCurrentMonster() const;
	void visualizeExtraInfo() const;
public:
	Maze();
	Maze(const Maze& other) = delete;
	Maze& operator=(const Maze& other) = delete;
	~Maze();


	void setHero(const Hero& hero);
	bool isOnItem() const;
	bool isOnTheExit() const;
	void generateLevel();
	const Pair<int, int> getHeroCords() const;
	bool moveHero(int x, int y);
	void visualizeCurrentState() const;
	bool pickUpItem();
	void beginBattle(Monster& monster) const;
	void toggleExtraData();
	void healthRegen();
};