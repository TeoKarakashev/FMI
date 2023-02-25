#pragma once
#include "../Hero.h"
class Warrior : public Hero {

public:
	Warrior();
	Warrior(size_t power, size_t mana, size_t health, size_t level,
		const Inventory& inventory /*const String& representation*/);
};
