template<typename T>
class LinkedQueue {

	struct Node {
		T data;
		Node* next;

		Node(const T& data) {
			this->data = data;
			next = nullptr;
		}
	};

	Node* head;
	Node* tail;

	void free();
	void copyFrom(const LinkedQueue<T>& other);

public:
	LinkedQueue();
	LinkedQueue(const LinkedQueue<T>& other);
	LinkedQueue<T>& operator=(const LinkedQueue<T>& other);
	~LinkedQueue();

	void enqueue(const T& el);
	T dequeue();
	const T& front() const;
	bool isEmpty() const;
};

template<typename T>
inline void LinkedQueue<T>::free() {
	Node* iter = head;
	while (iter != nullptr ){
		Node* prev = iter;
		iter = iter->next;
		delete prev;
	}
	head = tail = nullptr;
}

template<typename T>
inline void LinkedQueue<T>::copyFrom(const LinkedQueue<T>& other) {

	head = tail = nullptr;
	if (other.isEmpty()) {
		return;
	}
	Node* iter = other.head;
	while (iter){
		enqueue(iter->data);
		iter = iter->next;
	}
}

template<typename T>
inline LinkedQueue<T>::LinkedQueue() {
	head = tail = nullptr;
}

template<typename T>
inline LinkedQueue<T>::LinkedQueue(const LinkedQueue<T>& other) {
	copyFrom(other);
}

template<typename T>
inline LinkedQueue<T>& LinkedQueue<T>::operator=(const LinkedQueue<T>& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}

	return *this;
}

template<typename T>
inline LinkedQueue<T>::~LinkedQueue() {
	free();
}

template<typename T>
inline void LinkedQueue<T>::enqueue(const T& el) {
	if (isEmpty()) {
		head = tail = new Node(el);
	}
	else {
		Node* newNode = new Node(el);
		tail->next = newNode;
		tail = newNode;
	}
}

template<typename T>
inline T LinkedQueue<T>::dequeue()
{
	if (isEmpty()) {
		throw std::runtime_error("The LinkedQueue is empty");
	}
	else if (head == tail) {
		T removedData = head->data;
		Node* toDelete = head;
		head = tail = nullptr;
		delete toDelete;
		return removedData;
	}
	T removedData = head->data;
	Node* toDelete = head;
	head = head->next;
	delete toDelete;
	return removedData;
}

template<typename T>
inline const T& LinkedQueue<T>::front() const {
	if (isEmpty()) {
		throw std::runtime_error("The LinkedQueue is empty");
	}
	return head->data;
}

template<typename T>
inline bool LinkedQueue<T>::isEmpty() const {
	return head == nullptr && tail == nullptr;
}
