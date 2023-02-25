#pragma once


template<typename T>
class St {
	struct Node {
		T data;
		Node* next;

		Mode(T data) {
			this->data = data;
			next = nullptr;
		}
	};

	Node* topNode;
	size_t size;

	void free();
	void copyFrom(const St<T>& other);


public:

	St();
	St(const St<T>& other);
	St<T>& operator=(const St<T>& other);
	~St();

	void push(const T& data);
	void pop();
	const T& top() const;
	bool empty();


};

template<typename T>
inline void St<T>::free() {
	Node* iter = topNode;
	while (!iter) {
		Node* toDel = iter;
		iter = iter->next;
		delete toDel;
	}
	topNode = nullptr;
	size = 0;
}

template<typename T>
inline void St<T>::copyFrom(const St<T>& other) {
	topNode = nullptr;
	if (other->topNode == nullptr) {
		return;
	}
	Node* otherIter = other.topNode;
	topNode = new Node(otherIter->data);
	Node* thisIter = topNode;

	do {
		otherIter = otherIter->next;
		if (otherIter) {
			thisIter->next = new Node(otherIter->data);
			thisIter = thisIter->next;
		}

	} while (otherIter);

	size = other.size;
}

template<typename T>
inline St<T>::St()
{
	size = 0;
	topNode = nullptr;
}

template<typename T>
inline St<T>::St(const St<T>& other)
{
	copyFrom(other);
}

template<typename T>
inline St<T>& St<T>::operator=(const St<T>& other)
{
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename T>
inline St<T>::~St()
{
	free();
}

template<typename T>
inline void St<T>::push(const T& data)
{
	if (empty()) {
		topNode = new Node(data);
	}
	else {
		Node* newNode = new Node(data);
		newNode->next = topNode;
		topNode = newNode;
	}
	size++;
}

template<typename T>
inline void St<T>::pop()
{
	if (empty()) {
		throw new std::exception("sad");
	}
	Node* toDel = topNode;
	topNode = topNode->next;
	delete toDel;
	size--;
}

template<typename T>
inline const T& St<T>::top() const
{
	if (empty()) {
		throw new std::exception("sad");
	}
	return topNode->data;
}

template<typename T>
inline bool St<T>::empty()
{
	return topNode == nullptr;
}
