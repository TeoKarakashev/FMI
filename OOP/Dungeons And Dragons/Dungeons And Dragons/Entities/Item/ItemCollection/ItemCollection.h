#pragma once
#include "../Item.h"
class ItemCollection {
	Item** data;
	size_t size;
	size_t capacity;

	void free();
	void copyFrom(const ItemCollection& other);
	void resize();
public:
	ItemCollection();
	ItemCollection(const ItemCollection& other) = delete;
	ItemCollection& operator=(const ItemCollection& other) = delete;
	~ItemCollection();

	void addItem(const Item& item);
	const Item* getAt(int index) const;
	void setAt(int index, const String& name, int percentage );
	void clear();
};
