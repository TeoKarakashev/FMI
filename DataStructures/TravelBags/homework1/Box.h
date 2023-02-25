#pragma once
#include <string>
#include <vector>

class Box {
	std::string name;
	std::vector<std::string> items;
	std::vector<Box*> boxes;

public:
	Box(std::string name);
	
	void insertBox(Box*& box);
	void insertBoxFront(Box*& box);
	void insertItem(const std::string& item);

	bool isEmpty();
	bool hasOnlyOneBox();

	std::vector<Box*>& getBoxes();
	
	std::vector<std::string>& getItems();

	std::string getName() const;

};


