#pragma once

template<typename T>
class Stack {
	T* data;
	size_t topIndex;
	size_t capacity;

	void free();
	void copyFrom(const Stack<T>& other);
	void resize();
	bool full();

public:
	Stack();
	Stack(const Stack& other);
	Stack<T>& operator=(const Stack<T>& other);
	~Stack();

	void push(const T& value);
	void pop();
	T& top();
	const T& top() const;
	bool empty() const;
};

template<typename T>
inline void Stack<T>::free() {
	delete[] data;
}

template<typename T>
inline void Stack<T>::copyFrom(const Stack<T>& other) {
	data = new T[other.capacity];
	topIndex = other.topIndex;
	for (size_t i = 0; i <= topIndex; i++) {
		data[i] = other.data[i];
	}
	capacity = other.capacity;
}

template<typename T>
inline void Stack<T>::resize() {
	capacity *= 2;
	T* temp = new T[capacity];
	for (size_t i = 0; i <= topIndex; i++){
		temp[i] = data[i];
	}
	free();
	data = temp;
}

template<typename T>
inline bool Stack<T>::full()
{
	return capacity == topIndex + 1;
}

template<typename T>
inline Stack<T>::Stack() {
	capacity = 4;
	topIndex = -1;
	data = new T[capacity];
}

template<typename T>
inline Stack<T>::Stack(const Stack& other) {
	copyFrom(other);
}

template<typename T>
inline Stack<T>& Stack<T>::operator=(const Stack<T>& other){
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename T>
inline Stack<T>::~Stack() {
	free();
}

template<typename T>
inline void Stack<T>::push(const T& value) {
	if (full()) {
		resize();
	}
	data[++topIndex] = value;
}

template<typename T>
inline void Stack<T>::pop(){
	if (empty()) {
		throw std::runtime_error("You can not delete the top element of an empty stack!");
	}
	topIndex--;
}

template<typename T>
inline T& Stack<T>::top() {
	if (empty()) {
		throw std::runtime_error("You can not get the top element of an empty stack!");
	}

	return data[topIndex];
}

template<typename T>
inline const T& Stack<T>::top() const {
	if (empty()) {
		throw std::runtime_error("You can not get the top element of an empty stack!");
	}

	return data[topIndex];
}

template<typename T>
inline bool Stack<T>::empty() const {
	return topIndex == -1;
}
