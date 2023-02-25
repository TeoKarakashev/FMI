#pragma once
#include "../Hero.h"
class Human : public Hero {

public:
	Human();
	Human(size_t power, size_t mana, size_t health, size_t level,
		const Inventory& inventory /*const String& representation*/);
};
