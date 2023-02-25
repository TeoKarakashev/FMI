#pragma once

template <typename T>
class DoubleLinkedList {
	struct Node {
		T data;
		Node* next;
		Node* prev;

		Node(T& data) {
			this->data = data;
			next = nullptr;
			prev = nullptr;
		}
	};

	void free();
	void copyFrom(const DoubleLinkedList<T>& other);

	Node* head;
	Node* tail;


	class Iterator {
	private:
		Node* current;
		Iterator(Node* node) : current{ node } {}

	public:
		Iterator& operator++() {
			if (current == nullptr) {
				return *this;
			}
			current = current->next;
			return current;
		}
		Iterator operator++(int) {
			if (current == nullptr) {
				return *this;
			}
			Iterator old = *this;
			current = current->next;
			return old;
		}
		
		Iterator& operator--() {
			if (current == nullptr) {
				return *this;
			}
			current = current->prev;
			return current;
		}
		Iterator operator--(int) {
			if (current == nullptr) {
				return *this;
			}
			Iterator old = *this;
			current = current->prev;
			return old;
		}

		const T& operator*() const {
			if (current == nullptr) {
				throw std::exception();
			}
			else {
				return current->data;
			}
		}
		
		bool operator==(const Iterator& other) const {
			return current == other.current;
		}
		bool operator!=(const Iterator& other) const {
			return !(*this == other);
		}
		
		friend class DoubleLinkedList<T>;
	};

public:
	DoubleLinkedList();
	DoubleLinkedList(const DoubleLinkedList<T>& other);
	DoubleLinkedList<T>& operator=(const DoubleLinkedList<T>& other);
	~DoubleLinkedList();

	void pushBack(const T& el);
	void pushFront(const T& el);
	void popBack();
	void popFront();
	const T& front() const;
	const T& back() const;
	bool isEmpty() const;


	Iterator begin();
	Iterator end();
};

template<typename T>
inline void DoubleLinkedList<T>::free() {
	Node* iter = head;
	while (iter != nullptr) {
		Node* toDel = iter;
		iter = iter->next;
		delete toDel;
	}
	head = tail = nullptr;
}

template<typename T>
inline void DoubleLinkedList<T>::copyFrom(const DoubleLinkedList<T>& other) {
	head = tail = nullptr;
	if (other.isEmpty()) {
		return;
	}
	Node* otherIter = other->head;
	while (otherIter != nullptr) {
		this->pushBack(otherIter->data);
		otherIter = otherIter->next;
	}
}

template<typename T>
inline DoubleLinkedList<T>::DoubleLinkedList() {
	head = nullptr;
	tail = nullptr;

}

template<typename T>
inline DoubleLinkedList<T>::DoubleLinkedList(const DoubleLinkedList<T>& other) {
	copyFrom(other);
}

template<typename T>
inline DoubleLinkedList<T>& DoubleLinkedList<T>::operator=(const DoubleLinkedList<T>& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename T>
inline DoubleLinkedList<T>::~DoubleLinkedList() {
	free();
}

template<typename T>
inline void DoubleLinkedList<T>::pushBack(const T& el) {
	Node* newNode = new Node(el);

	if (isEmpty()) {
		head = tail = newNode;
	}
	else {
		tail->next = newNode;
		newNode->prev = tail;
		tail = tail->next;
	}
}

template<typename T>
inline void DoubleLinkedList<T>::pushFront(const T& el) {
	Node* newNode = new Node(el);

	if (isEmpty()) {
		head = tail = newNode;
	}
	else {
		head->prev = newNode;
		newNode->next = head;
		head = head->prev;
	}

}

template<typename T>
inline void DoubleLinkedList<T>::popBack() {

	if (isEmpty()) {
		return;
	}
	if (head = tail) {
		delete head;
		head = tail = nullptr;
	}
	else {
		Node* toDel = tail;
		tail = tail->prev;
		delete toDel;
	}
}

template<typename T>
inline void DoubleLinkedList<T>::popFront() {
	if (isEmpty()) {
		return;
	}
	if (head = tail) {
		delete head;
		head = tail = nullptr;
	}
	else {
		Node* toDel = head;
		head = head->next;
		delete toDel;
	}
	
}

template<typename T>
inline const T& DoubleLinkedList<T>::front() const
{
	if (isEmpty()) {
		return;
	}
	return head->data;
}

template<typename T>
inline const T& DoubleLinkedList<T>::back() const
{
	if (isEmpty()) {
		return;
	}
	return tail->data;
}

template<typename T>
inline bool DoubleLinkedList<T>::isEmpty() const
{
	return head == nullptr;
}

template<typename T>
typename DoubleLinkedList<T>::Iterator DoubleLinkedList<T>::begin() {
	return Iterator(head);
}

template<typen

