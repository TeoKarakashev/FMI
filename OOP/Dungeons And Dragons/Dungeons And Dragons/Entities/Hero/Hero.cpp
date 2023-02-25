#include "Hero.h"

Hero::Hero(size_t power, size_t mana, size_t health) {
	this->power = power;
	this->mana = mana;
	this->maxHealth = health;
	this->currentHealth = health;
	this->level = 1;
}

Hero::Hero(size_t power, size_t mana, size_t health, size_t level, const Inventory& inventory /*const String& representation*/) {
	this->power = power;
	this->mana = mana;
	this->maxHealth = health;
	this->currentHealth = health;
	this->level = level;
	this->inventory = inventory;
}

void Hero::setPower(size_t power) {
	this->power = power;
}

void Hero::setMana(size_t mana) {
	this->mana = mana;
}

void Hero::setHealth(size_t health) {
	this->maxHealth = health;
}

void Hero::setCurrentHealth(size_t health) {
	currentHealth = health;
}

void Hero::setLevel(size_t level) {
	this->level = level;
}

void Hero::setWeapon(const Weapon& weapon) {
	this->inventory.setWeapon(weapon);
}

void Hero::setArmour(const Armour& armour) {
	this->inventory.setArmour(armour);
}

void Hero::setSpell(const Spell& spell) {
	this->inventory.setSpell(spell);
}

//void Hero::setRepresentation(const String& path) {
//	this->representation = path;
//}

void Hero::levelUp(size_t powerIncrease, size_t manaIncrease, size_t healthIncrease) {
	level++;
	maxHealth += healthIncrease;
	currentHealth = maxHealth;
	mana += manaIncrease;
	power += powerIncrease;
}

size_t Hero::getPower() const
{
	return power;
}


size_t Hero::getMana() const
{
	return mana;
}

size_t Hero::getHealth() const
{
	return maxHealth;
}

size_t Hero::getCurrentHealth() const
{
	return currentHealth;
}

size_t Hero::getLevel() const
{
	return level;
}

const Weapon& Hero::getWeapon() const {
	return inventory.getWeapon();
}

const Optional<Armour>& Hero::getArmour() const {
	return inventory.getArmour();
}

const Spell& Hero::getSpell() const {
	return inventory.getSpell();
}

void Hero::healthRegen() {
	if (currentHealth < maxHealth) {
		currentHealth++;
	}
}

//const String& Hero::getRepresentation() const {
//	return representation;
//}
