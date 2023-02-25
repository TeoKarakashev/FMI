#include "Weapon.h"

Weapon::Weapon(const String& name, size_t percentage) : Item(name, percentage) {
}

Item* Weapon::clone() const{
	Item* newObj = new Weapon(*this);
	return newObj;
}
