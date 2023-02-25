#include "HeroContainer.h"

void HeroContainer::free() {
	delete[] hero;
}

void HeroContainer::copyFrom(const HeroContainer& other) {
	hero = new Hero(other.hero->getPower(), other.hero->getMana(), other.hero->getHealth());

}

HeroContainer::HeroContainer() {
	hero = nullptr;
}


HeroContainer::~HeroContainer() {
	free();
}

void HeroContainer::addHero(const Hero& hero) {
	if (this->hero != nullptr) {
		throw std::exception("Hero container is full");
	}
	this->hero = new Hero(hero);
}

const Hero* HeroContainer::getHero() const
{
	return hero;
}

Hero* HeroContainer::getHero()
{
	return hero;
}
