#pragma once
#include<iostream>
//! Helper class that helps working with String
class String {
	//! Storage for all the chars in the String
	char* vals;
	//! Size of the string
	size_t size;

	//! Copies the other string the the current
	void copyFrom(const String& other);
	//! deletes the string
	void free();

public:
	//! Default constructor for the class
	String();
	//! Constructor with parameters
	String(const char* vals);
	//! Copy Contructor 
	String(const String& other);
	//! Destructor
	~String();

	//! operator =
	String& operator=(const String& other);
	//! Implementation for the [] operator
	char operator[](int index) const;
	//! Getter for the size
	size_t getSize() const;
	//! Returns a substring of the current string bettween start and end index 
	String substr(int startIndex, int endIndex) const;

	//! Returns which string bigger using ASCII comparisson
	int strcmp(const String& other) const;
	//! Parses the String to integer
	int atoi(const String& str) const;
	//! concats two string
	void strcat(const String& str);
	//! replaces character of a string with another 
	void replaceAt(int index, char ch);
	//! converst a single character to integer
	int convertCharToInt(char ch) const;
	//! converts a single character to its Upper cas
	char toUpper(const char ch) const;

	//! Implementation for the == operator
	bool operator==(const String& other) const;
	//! Implementation for the != operator
	bool operator!=(const String& other) const;
	//! Implementation for String << operator
	friend std::ostream& operator<<(std::ostream& stream, const String& str);
	//! Implementation for the >> operator
	friend std::istream& operator>>(std::istream& stream, String& str);
};