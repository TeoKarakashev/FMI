#pragma once
#include <string>
#include <map>

class Vertice {

	std::string name;
	std::map<Vertice, int> adjacent;

public:

	Vertice() = default;

	Vertice(std::string name);

	std::string getName() const;

	std::map<Vertice, int> getAdjacentsList() const;

	void addEdge(const Vertice& end, int weight);

	bool operator<(const Vertice& other) const;
};
