#include <string>
#include <iostream>
struct Node {
	std::string data;
	Node* next;
	Node* skip;

	Node(const std::string& data, Node* n = nullptr, Node* s = nullptr) : data{ data }, next{ n }, skip{ s } {}
};


class SkipList {

private:

	Node* head;
	Node* tail;
	int size;
	void free();

public:

	SkipList();
	SkipList(const SkipList& other) = delete;
	SkipList& operator=(const SkipList& other) = delete;
	~SkipList();

	void add(const std::string& data);
	void addSkip(const std::string& from, const std::string& to);
	bool empty() const;
	void print() const;
	Node* getHead() const;
	Node* getTail() const;
	int getSize() const;
	Node* getByData(const std::string& data) const;
};











