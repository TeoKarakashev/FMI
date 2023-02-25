#include "Armour.h"

Armour::Armour(const String& name, size_t percentage) : Item(name, percentage) {

}

Item* Armour::clone() const{
	Item* newObj = new Armour(*this);
	return newObj;
}
