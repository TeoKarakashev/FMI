#include "Box.h"

Box::Box(std::string name) {
	this->name = name;
	items = std::vector<std::string>();
	boxes = std::vector<Box*>();
}


std::vector<Box*>& Box::getBoxes() {
	return boxes;
}

std::vector<std::string>& Box::getItems() {
	return items;
}

std::string Box::getName() const{
	return name;
}


void Box::insertBox(Box*& box) {
	boxes.push_back(box);
}

void Box::insertBoxFront(Box*& box) {
	boxes.insert(boxes.begin(), box);
}

void Box::insertItem(const std::string& item) {
	items.push_back(item);
}

bool Box::isEmpty() {
	return items.size() == 0 && boxes.size() == 0;
}


bool Box::hasOnlyOneBox() {
	return boxes.size() == 1 && items.size() == 0;
}

