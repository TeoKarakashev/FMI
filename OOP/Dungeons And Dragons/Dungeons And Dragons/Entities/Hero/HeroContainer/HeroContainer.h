#pragma once
#include "../Hero.h"
class HeroContainer {

	Hero* hero;

	void free();
	void copyFrom(const HeroContainer& other);

public:
	HeroContainer();
	HeroContainer(const HeroContainer& other) = delete;
	HeroContainer& operator=(const HeroContainer& other) = delete;
	~HeroContainer();

	void addHero(const Hero& hero);
	const Hero* getHero() const;
	Hero* getHero();
};
