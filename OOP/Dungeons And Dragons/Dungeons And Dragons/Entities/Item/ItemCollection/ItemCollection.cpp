#include "ItemCollection.h"

void ItemCollection::free() {
	for (int i = 0; i < size; i++){
		delete data[i];
	}
	delete[] data;
}

void ItemCollection::copyFrom(const ItemCollection& other){
	data = new Item * [other.capacity];
	size = other.size;
	capacity = other.capacity;

	for (size_t i = 0; i < size; i++)
	{
		data[i] = other.data[i]->clone();
	}
}

void ItemCollection::resize() {
	Item** newCollection = new Item * [capacity *= 2];
	for (size_t i = 0; i < size; i++)
		newCollection[i] = data[i];
	delete[] data;
	data = newCollection;
}

ItemCollection::ItemCollection() {
	data = new Item * [2];
	capacity = 2;
	size = 0;
}

ItemCollection::~ItemCollection() {
	free();
}

void ItemCollection::addItem(const Item& item) {
	if (size == capacity) {
		resize();
	}
	data[size++] = item.clone();
}

const Item* ItemCollection::getAt(int index) const {
	if (index < 0 || index > size - 1) {
		throw std::exception("Invalid index");
	}
	return data[index];
}

void ItemCollection::setAt(int index, const String& name, int percentage) {
	if (index < 0 || index > size - 1) {
		throw std::exception("Invalid index");
	}
	data[index]->setName(name);
	data[index]->setPercent(percentage);

}
void ItemCollection::clear() {
	size = 0;
}
