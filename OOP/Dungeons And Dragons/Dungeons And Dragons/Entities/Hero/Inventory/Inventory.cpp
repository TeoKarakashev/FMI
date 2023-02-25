#include "Inventory.h"

Inventory::Inventory() : armour(), weapon("normal sword", 20), spell("fire ball", 20) {
}

Inventory::Inventory(const Armour& armour, const Weapon& weapon, const Spell& spell) : armour(armour), weapon(weapon), spell(spell) {
}

void Inventory::setArmour(const Armour& armour) {
	this->armour = armour;
}

void Inventory::setWeapon(const Weapon& weapon) {
	this->weapon = weapon;
}

void Inventory::setSpell(const Spell& spell) {
	this->spell = spell;
}

const Spell& Inventory::getSpell() const {
	return this->spell;
}

const Optional<Armour>& Inventory::getArmour() const {
	return this->armour;
}

const Weapon& Inventory::getWeapon() const {
	return this->weapon;
}
