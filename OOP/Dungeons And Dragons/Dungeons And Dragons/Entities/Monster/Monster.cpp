#include "Monster.h"

Monster::Monster() :power(25), mana(25), maxHealth(50), currentHealth(50), shield(15) {
}

Monster::Monster(size_t power, size_t mana, size_t maxHealth, size_t currentHealth, size_t shield) {
	this->power = power;
	this->mana = mana;
	this->maxHealth = maxHealth;
	this->currentHealth = currentHealth;
	this->shield = shield;

}

void Monster::setPower(size_t power) {
	this->power = power;
}

void Monster::setMana(size_t mana) {
	this->mana = mana;
}

void Monster::setHealth(size_t maxHealth) {
	this->maxHealth = maxHealth;
}

void Monster::setCurrentHealth(size_t currentHealth) {
	this->currentHealth = currentHealth;
}

void Monster::setShield(size_t shield) {
	this->shield = shield;
}

size_t Monster::getPower() const
{
	return power;
}

size_t Monster::getMana() const
{
	return mana;
}

size_t Monster::getHealth() const
{
	return maxHealth;
}

size_t Monster::getCurrentHealth() const
{
	return currentHealth;
}

size_t Monster::getShield() const
{
	return shield;
}
