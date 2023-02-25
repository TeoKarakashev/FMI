#pragma once
#include "../Item.h"

class Weapon : public Item {


public:
	Weapon(const String& name, size_t percentage);
	Item* clone() const override;
};
