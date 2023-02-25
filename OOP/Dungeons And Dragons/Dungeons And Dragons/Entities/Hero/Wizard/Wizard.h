#pragma once
#include "../Hero.h"
class Wizzard : public Hero {

public:
	Wizzard();
	Wizzard(size_t power, size_t mana, size_t health, size_t level,
		const Inventory& inventory /*const String& representation*/);
};
