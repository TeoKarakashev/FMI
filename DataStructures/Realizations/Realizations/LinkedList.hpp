template <typename T >
class LinkedList {
	
private:
	struct Node {
		T data;
		Node* next;

		Node(const T& data, Node* n = nullptr) {
			this->data = data;
			this->next = n;
		}
	};

	void free();
	void copyFrom(const LinkedList<T>& other);
	void move(LinkedList<T>&& other);

	Node* head;
	Node* tail;

	class Iterator {
	private:
		Node* currentNode;

		Iterator(Node* node) :  currentNode {node} ;
	public:

		Iterator& operator++() {
			if (currentNode == nullptr) {
				return *this;
			}
			currentNode = currentNode->next;
			return *this;
		}

		Iterator operator++(int) {
			if (currentNode == nullptr) {
				return *this;
			}
			Iterator prev = *this;
			++(*this);
			return prev;
		}

		T& operator*() {
			return currentNode->data;
		}

		bool operartor== (const Iterator& other) const {
			return currentNode == other.currentNode;
		}

		bool operator!=(const Iterator& other) const {
			return !(*this == other);
		}


		friend class LinkedList;
	};

	class ConstIterator {
	private:
		const Node* currentNode;

		ConstIterator(Node* node) : currentNode{ node };
	public:

		ConstIterator& operator++() {
			if (currentNode == nullptr) {
				return *this;
			}
			currentNode = currentNode->next;
			return *this;
		}

		ConstIterator operator++(int) {
			if (currentNode == nullptr) {
				return *this;
			}
			ConstIterator prev = *this;
			++(*this);
			return prev;
		}

		const T& operator*() const {
			return currentNode->data;
		}

		bool operartor == (const Iterator & other) const {
			return currentNode == other.currentNode;
		}

		bool operator!=(const Iterator& other) const {
			return !(*this == other);
		}


		friend class LinkedList;
	};

public:
	LinkedList();
	LinkedList(const LinkedList<T>& other);
	LinkedList<T>& operator=(const LinkedList<T>& other);
	~LinkedList();

	LinkedList(LinkedList<T>&& other);
	LinkedList<T>& operator=(LinkedList<T>&& other);

	Iterator begin() {
		return Iterator(head);
	}

	Iterator end() {
		return Iterator(nullptr);
	}

	ConstIterator begin() const{
		return Iterator(head);
	}

	ConstIterator end() const{
		return Iterator(nullptr);
	}
x
	ConstIterator cbegin() {
		return ConstIterator(head);
	}

	ConstIterator cend() {
		return ConstIterator(nullptr);
	}

	void pushBack(const T&);
	void popBack();

	void pushFront(const T&);
	void popFront();

	const T& front() const;
	const T& back() const;

	bool empty() const;
};

template<typename T>
inline void LinkedList<T>::free() {
	while (!empty()){
		Node* toDelete = head;
		head = head->next;
		delete toDelete;
	}
}

template<typename T>
inline void LinkedList<T>::copyFrom(const LinkedList<T>& other) {
	head = tail = nullptr;
	if (other.empty()) {
		return;
	}

	Node* iter = other.head;
	while (iter){
		Node* el = new Node(iter->data, iter->next);
		if (head == nullptr) {
			head = tail = el;
		}
		else {
			tail->next = el;
			tail = el;
		}
		iter = iter->next;
	}

}

template<typename T>
inline void LinkedList<T>::move(LinkedList<T>&& other) {
	head = other.head;
	tail = other.tail;

	other.head = other.tail = nullptr;
}

template<typename T>
inline bool LinkedList<T>::empty() const
{

	return head == nullptr && tail == nullptr;
}

template<typename T>
inline LinkedList<T>::LinkedList() {
	head = tail = nullptr;
}

template<typename T>
inline LinkedList<T>::LinkedList(const LinkedList<T>& other) {
	copyFrom(other);
}

template<typename T>
inline LinkedList<T>& LinkedList<T>::operator=(const LinkedList<T>& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename T>
inline LinkedList<T>::~LinkedList() {
	free();
}

template<typename T>
inline LinkedList<T>::LinkedList(LinkedList<T>&& other) {
	move(other);
}

template<typename T>
inline LinkedList<T>& LinkedList<T>::operator=(LinkedList<T>&& other) {
	if (this != &other) {
		free();
		move(other);
	}
	return *this;
}



template<typename T>
inline void LinkedList<T>::pushBack(const T&)
{
	Node* toAdd = new Node(el);
	if (empty()) {
		head = tail = toAdd;
	}
	else {
		tail->next = toAdd;
		tail = toAdd;
	}
}

template<typename T>
inline void LinkedList<T>::popBack() {
	if (empty()) {
		throw std::runtime_error("Tried to pop back on empty list!");
	}
	if (head == tail) {
		delete tail;
		head = tail = nullptr;
		return;
	}

	Node* iter = head;
	
	while (iter->next != tail){
		iter = iter->next;
	}

	delete tail;
	tail = iter;
	tail->next = nullptr;
}

template<typename T>
inline void LinkedList<T>::pushFront(const T& el) {
	Node* toAdd = new Node(el);
	if (empty()) {
		head = tail = toAdd;
	}
	else {
		toAdd->next = head;
		head = toAdd;
	}

}

template<typename T>
inline void LinkedList<T>::popFront() {
	if (empty()) {
		throw std::runtime_error("Tried to pop front on empty list!");
	}
	if (head == tail) {
		delete tail;
		head = tail = nullptr;
		return;
	}

	Node* toDelete = head;
	head = head->next;
	delete toDelete;
}

template<typename T>
const T& LinkedList<T>::front() const {
	if (empty())
		throw std::runtime_error("Empty list!");

	return head->data;
}

template<typename T>
const T& LinkedList<T>::back() const {
	if (empty())
		throw std::runtime_error("Empty list!");

	return tail->data;
}