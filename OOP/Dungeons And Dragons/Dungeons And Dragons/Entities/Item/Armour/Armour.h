#pragma once
#include "../Item.h"
class Armour : public Item{

public:
	Armour(const String& name, size_t percentage);
	Item* clone() const override;
};
