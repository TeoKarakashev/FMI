#pragma once
#include "../../Helpers/String/String.h"

class Item {
	String name;
	size_t percentIncrease;
	//unsigned level;

public:

	Item(const String& name, size_t percent);

	const String& getName() const;
	size_t getPercentIncrease() const;
	//size_t getLevel() const;

	void setName(const String& name);
	void setPercent(size_t percent);
	//void setLevel(unsigned level);
	virtual Item* clone() const = 0;
	virtual ~Item() = default;
};
