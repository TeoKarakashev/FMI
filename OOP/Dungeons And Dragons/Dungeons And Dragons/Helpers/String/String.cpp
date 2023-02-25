#include "String.h"
#include <iostream>
#pragma warning(disable: 4996)

void String::copyFrom(const String& other) {
	vals = new char[other.size + 1];
	strcpy(vals, other.vals);
	size = other.size;
}

void String::free() {
	delete[] vals;
}

String::String() {
	vals = new char[1];
	vals[0] = '\0';
	size = 0;
}

String::String(const char* vals) {
	size = strlen(vals);
	this->vals = new char[size + 1];
	strcpy(this->vals, vals);
}

String::String(const String& other) {
	copyFrom(other);
}

String& String::operator=(const String& other) {

	if (this != &other) {
		free();
		copyFrom(other);
	}
	return *this;
}

char String::operator[](int index) const {
	if (index < 0 || index >= size) {
		throw std::exception("Invalid index");
	}
	return vals[index];
}

String::~String() {
	free();
}

size_t String::getSize() const {
	return size;
}

String String::substr(int startIndex, int endIndex) const {
	if (endIndex < startIndex) {
		throw std::exception("substr cannot have negative size");
	}

	char* substr = new char[size + 1];
	int strSize = 0;
	for (int i = startIndex; i < endIndex; i++) {
		substr[strSize++] = vals[i];
	}
	substr[strSize++] = '\0';
	String temp(substr);
	delete[] substr;
	return temp;
}

int String::strcmp(const String& other) const {
	size_t minSize = std::min(this->size, other.size);
	for (int i = 0; i < minSize; i++) {
		if (this->vals[i] != other.vals[i]) {
			return this->vals[i] > other.vals[i] ? 1 : -1;
		}
	}
	if (this->size == other.size) {
		return 0;
	}
	else if (this->size > other.size) {
		return 1;
	}
	return -1;

}

int String::atoi(const String& str) const {
	int res = 0;
	for (int i = 0; i < str.getSize(); i++) {
		res *= 10;
		res += convertCharToInt(str[i]);
	}
	return res;
}

void String::strcat(const String& str) {

	char* newStr = new char[size + str.size + 1];
	for (int i = 0; i < size; i++) {
		newStr[i] = vals[i];
	}

	for (int i = size; i < str.size + size; i++) {
		newStr[i] = str.vals[i - size];
	}
	newStr[str.size + size] = '\0';
	size = strlen(newStr);
	delete[] vals;
	vals = newStr;
}

void String::replaceAt(int index, const char ch) {

	if (index < 0 || index > size - 1) {
		throw std::exception("Invalid Index");
	}
	vals[index] = ch;
}

int String::convertCharToInt(const char ch) const
{
	if (ch < '0' || ch > '9') {
		throw std::exception("invalid char");
	}
	return ch - '0';
}

char String::toUpper(const char ch) const {
	if (ch >= 'a' && ch <= 'z') {
		return (ch - 'a') + 'A';
	}
	return ch;
}

bool String::operator==(const String& other) const {
	return strcmp(other.vals) == 0;
}

bool String::operator!=(const String& other) const
{
	return strcmp(other.vals) != 0;
}

std::ostream& operator<<(std::ostream& stream, const String& str) {
	stream << str.vals;
	return stream;
}

std::istream& operator>>(std::istream& stream, String& str) {

	delete[] str.vals;
	char buffer[1024];
	stream.getline(buffer, 1024);
	if (buffer[0] == '\0') {
		stream.getline(buffer, 1024);
	}
	str.size = strlen(buffer);
	str.vals = new char[str.size + 1];
	strcpy(str.vals, buffer);

	return stream;
}
