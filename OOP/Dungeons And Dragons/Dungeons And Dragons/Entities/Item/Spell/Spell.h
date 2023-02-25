#pragma once
#include "../Item.h"
class Spell : public Item {

public:
	Spell(const String& name, size_t percentage);
	Item* clone() const override;
};
