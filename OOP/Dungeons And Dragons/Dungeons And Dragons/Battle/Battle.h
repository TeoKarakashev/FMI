#pragma once
#include "../Entities/Hero/Hero.h"
#include "../Entities/Monster/Monster.h"
class Battle {

public:
	bool fight(const Hero* hero, Monster& monster);
};
