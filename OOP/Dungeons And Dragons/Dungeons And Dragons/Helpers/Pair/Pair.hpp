//Taken from https://github.com/stoychoX/Alice-in-wonderland/
#pragma once
template<class T, class U>
class Pair {
public:
	T first;
	U second;

	//construct:
	Pair();
	Pair(const T& _first, const U& _second);
	Pair(const Pair<T, U>& other);

	//operator= && operator==
	Pair& operator=(const Pair<T, U>& other);
	bool operator==(const  Pair<T, U>& other);

	//helpers:
	Pair make_pair(const T& _valFirst, const U& _valSecond) const;
	void set_pair(const T& _valFirst, const U& _valSecond) ;

	const T c_first() const;
	const U c_second() const;

	//destruct:
	~Pair();
private:
	void copy(const Pair<T, U>& other);
};

template<class T, class U>
inline Pair<T, U>::~Pair() {}

template<class T, class U>
inline void Pair<T, U>::copy(const Pair<T, U>& other) {
	this->first = other.first;
	this->second = other.second;
}

template<class T, class U>
inline Pair<T, U>::Pair() : first(T()), second(U()) {}

template<class T, class U>
inline Pair<T, U>::Pair(const T& _first, const U& _second) : first(_first), second(_second) {}

template<class T, class U>
inline Pair<T, U>::Pair(const Pair<T, U>& other) {
	copy(other);
}

template<class T, class U>
inline Pair<T, U>& Pair<T, U>::operator=(const Pair<T, U>& other)
{
	if (this != &other)
		copy(other);

	return *this;
}

template<class T, class U>
inline bool Pair<T, U>::operator==(const Pair<T, U>& other) {
	return ((this->first == other.first) && (this->second == other.second));
}

template<class T, class U>
inline Pair<T, U> Pair<T, U>::make_pair(const T& _valFirst, const U& _valSecond) const {
	return Pair(_valFirst, _valSecond);
}

template<class T, class U>
inline void Pair<T, U>::set_pair(const T& _valFirst, const U& _valSecond)  {
	this->first = _valFirst;
	this->second = _valSecond;
}

template<class T, class U>
inline const T Pair<T, U>::c_first() const {
	return this->first;
}

template<class T, class U>
inline const U Pair<T, U>::c_second() const {
	return this->second;
}
