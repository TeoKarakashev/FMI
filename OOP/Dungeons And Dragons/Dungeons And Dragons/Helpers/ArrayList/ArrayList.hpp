#pragma once
#include <iostream>
template<typename E>
class ArrayList {
	E* data;
	size_t size;
	size_t capacity;

	void free();
	void copyFrom(const ArrayList& other);
	void resize();
	void shiftRight(int index);
	void shiftLeft(int index);
public:
	ArrayList();
	ArrayList(const ArrayList& other);
	ArrayList<E>& operator=(const ArrayList& other);
	~ArrayList();

	bool add(const E& e);
	bool add(E&& e);
	void add(int index, const E& e);
	bool addAll(const ArrayList<E>& other);
	bool contains(const E& e) const;
	bool isEmpty() const;
	int indexOf(const E& e) const;
	size_t Size() const;
	E removeAt(int index);
	bool remove(const E& e);
	void replaceAt(int index, const E& e);
	void clear();
	E& operator[](int index);
	E operator[](int index) const;
};

template<typename E>
inline void ArrayList<E>::free() {
	delete[] data;
}

template<typename E>
inline void ArrayList<E>::copyFrom(const ArrayList& other) {
	data = new E[other.capacity];
	for (int i = 0; i < size; i++) {
		data[i] = other.data[i];
	}
	size = other.size;
	capacity = other.capacity;
}

template<typename E>
inline void ArrayList<E>::resize() {
	capacity *= 2;
	E* newData = new E[capacity];
	for (int i = 0; i < size; i++) {
		newData[i] = data[i];
	}
	delete[] data;
	data = newData;
}

template<typename E>
inline void ArrayList<E>::shiftRight(int index) {
	for (int i = size - 1; i > index; i--) {
		data[i + 1] = data[i];
	}
}

template<typename E>
inline void ArrayList<E>::shiftLeft(int index) {
	for (int i = index; i < size - 1; i++) {
		data[i] = data[i + 1];
	}
}

template<typename E>
inline ArrayList<E>::ArrayList() {
	capacity = 4;
	data = new E[capacity];
	size = 0;
}

template<typename E>
inline ArrayList<E>::ArrayList(const ArrayList& other) {
	copyFrom(other);
}

template<typename E>
inline ArrayList<E>& ArrayList<E>::operator=(const ArrayList& other) {
	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

template<typename E>
inline ArrayList<E>::~ArrayList() {
	free();
}

template<typename E>
inline bool ArrayList<E>::add(const E& e) {
	if (size == capacity) {
		resize();
	}
	data[size++] = e;
	return true;
}

template<typename E>
inline bool ArrayList<E>::add(E&& e) {
	if (size == capacity) {
		resize();
	}
	data[size++] = std::move(e);
	return true;
}

template<typename E>
inline void ArrayList<E>::add(int index, const E& e) {
	if (index < 0 || index > size) {
		throw std::exception("Invalid index");
	}
	if (size == capacity) {
		resize();
	}
	shiftRight(index);
	data[index] = e;
	size++;
}

template<typename E>
inline bool ArrayList<E>::addAll(const ArrayList<E>& other) {
	for (int i = 0; i < other.size; i++) {
		add(other.data[i]);
	}
	return true;
}

template<typename E>
inline bool ArrayList<E>::contains(const E& e) const {
	for (int i = 0; i < size; i++) {
		if (data[i] == e) {
			return true;
		}
	}
	return false;
}

template<typename E>
inline bool ArrayList<E>::isEmpty() const {
	return size == 0;
}

template<typename E>
inline int ArrayList<E>::indexOf(const E& e) const {
	for (int i = 0; i < size; i++) {
		if (data[i] == e) {
			return i;
		}
	}
	return -1;
}

template<typename E>
inline size_t ArrayList<E>::Size() const {
	return size;
}

template<typename E>
inline E ArrayList<E>::removeAt(int index) {
	if (index < 0 || index >= size) {
		throw std::exception("Invalid index");
	}
	E el = data[index];
	shiftLeft(index);
	size--;
	return el;
}

template<typename E>
inline bool ArrayList<E>::remove(const E& e) {
	for (int i = 0; i < size; i++){
		if (data[i] == e) {
			shiftLeft(i);
			size--;
			return true;
		}
	}
	return false;
}

template<typename E>
inline void ArrayList<E>::replaceAt(int index, const E& e) {
	if (index < 0 || index >= size) {
		throw std::exception("Invalid index");
	}
	data[index] = e;
}

template<typename E>
inline void ArrayList<E>::clear() {
	size = 0;
}

template<typename E>
inline E& ArrayList<E>::operator[](int index) {
	if (index < 0 || index >= size) {
		throw std::exception("Invalid index");
	}

	return data[index];
}

template<typename E>
inline E ArrayList<E>::operator[](int index) const
{
	if (index < 0 || index >= size) {
		throw std::exception("Invalid index");
	}

	return data[index];
}
