#include "Spell.h"

Spell::Spell(const String& name, size_t percentage) :Item(name, percentage) {
}

Item* Spell::clone() const {   
	Item* newObj = new Spell(*this);
	return newObj;
}
