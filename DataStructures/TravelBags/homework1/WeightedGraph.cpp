#include "WeightedGraph.h"

Vertice::Vertice(std::string name) {
	this->name = name;
}


std::string Vertice::getName() const {
	return name;
}

std::map<Vertice, int> Vertice::getAdjacentsList() const {
	return adjacent;
}

void Vertice::addEdge(const Vertice& end, int weight) {
	adjacent[end] = weight;
}

bool Vertice::operator<(const Vertice& other) const{
	return this->name < other.name;
}
