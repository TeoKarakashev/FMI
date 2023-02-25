#pragma once

template<typename T>
class LinkedStack {
	struct Node {

		T data;
		Node* next;
		Node(T data)
		{
			this->data = data;
			next = nullptr;
		}
	};

	Node* topNode;
	size_t size;

	void copyFrom(const LinkedStack<T>& other);
	void free();

public:
	LinkedStack();
	LinkedStack(const LinkedStack<T>& other);
	LinkedStack<T>& operator=(const LinkedStack<T>& other);
	~LinkedStack();

	void push(const T& value);
	T pop();
	T& top();
	const T& top() const;
	bool isEmpty() const;
};

template<typename T>
inline void LinkedStack<T>::copyFrom(const LinkedStack<T>& other) {
	topNode = nullptr;
	if (other.isEmpty())
		return;

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
inline void LinkedStack<T>::free() {
	Node* iter = topNode;
	while (iter != nullptr) {
		Node* prev = iter;
		iter = iter->next;
		delete prev;
	}
	
}

template<typename T>
inline LinkedStack<T>::LinkedStack() {
	topNode = nullptr;
	size = 0;
}

template<typename T>
inline LinkedStack<T>::LinkedStack(const LinkedStack<T>& other) {
	copyFrom(other);
}

template<typename T>
inline LinkedStack<T>& LinkedStack<T>::operator=(const LinkedStack<T>& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename T>
inline LinkedStack<T>::~LinkedStack() {
	free();
}

template<typename T>
inline void LinkedStack<T>::push(const T& value) {
	Node* newNode = new Node(value);
	if (isEmpty())
		topNode = newNode;
	else {
		newNode->next = topNode;
		topNode = newNode;
	}
	size++;
}

template<typename T>
inline T LinkedStack<T>::pop() {
	if (isEmpty()) {
		throw std::runtime_error("The LinkedStack is empty");
	}

	T data = topNode->data;
	Node* toDelete = topNode;
	topNode = topNode->next;
	delete toDelete;
	size--;
	return data;

}

template<typename T>
inline T& LinkedStack<T>::top() {
	if (isEmpty) {
		throw std::runtime_error("The LinkedStack is empty");
	}
	return topNode->data;
}

template<typename T>
inline const T& LinkedStack<T>::top() const {
	if (isEmpty) {
		throw std::runtime_error("The LinkedStack is empty");
	}
	return topNode->data;
}

template<typename T>
inline bool LinkedStack<T>::isEmpty() const {
	return size == 0;
}
