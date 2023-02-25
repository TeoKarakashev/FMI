#include "SkipList.h"

void SkipList::free() {
	while (head) {
		Node* toDelete = head;
		head = head->next;
		delete toDelete;
	}
	size = 0;
}


SkipList::SkipList() {
	head = tail = nullptr;
	size = 0;
}


SkipList::~SkipList() {
	free();
}


void SkipList::add(const std::string& data) {
	Node* toAdd = new Node(data);
	if (empty()) {
		head = tail = toAdd;
	}
	else {
		tail->next = toAdd;
		tail = toAdd;
	}
	size++;
}

void SkipList::addSkip(const std::string& from, const std::string& to) {
	Node* fromNode = head;
	while (fromNode && fromNode->data != from) {
		fromNode = fromNode->next;
	}
	if (!fromNode) {
		return;
	}
	Node* toNode = head;
	while (toNode && toNode->data != to) {
		toNode = toNode->next;
	}
	if (!toNode) {
		return;
	}
	fromNode->skip = toNode;
}



bool SkipList::empty() const {
	return head == nullptr && tail == nullptr;
}


void SkipList::print() const {
	Node* current = head;
	while (current) {
		std::cout << current->data << " ";
		if (current->skip != nullptr) {
			std::cout << "(" << current->skip->data << ") ";
		}
		current = current->next;
	}
	std::cout << std::endl;
}


Node* SkipList::getHead() const {

	return head;
}


Node* SkipList::getTail() const {
	return tail;
}

int SkipList::getSize() const {
	return size;
}


Node* SkipList::getByData(const std::string& data) const {
	Node* iter = head;
	while (iter != nullptr) {
		if (iter->data == data) {
			return iter;
		}
		iter = iter->next;
	}
	return nullptr;
}
