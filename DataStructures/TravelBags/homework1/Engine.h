#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include "SkipList.h"
#include "WeightedGraph.h"
#include <fstream>
#include <map>
#include <unordered_set>
#include "Box.h"

#pragma warning (disable : 4996)

class Engine {

private:

	SkipList cities;
	std::vector<Box*> boxesList;

	void freeBoxRec(Box* box) {
		for (int i = 0; i < box->getBoxes().size(); i++) {
			freeBoxRec(box->getBoxes()[i]);
		}
		delete box;
	}

	void freeBoxes() {
		for (size_t i = 0; i < boxesList.size(); i++) {
			freeBoxRec(boxesList[i]);
		}
		boxesList.clear();
	}
	
	Box* createBox(std::vector<std::string>& boxes, int& boxIndexToCreate) {
		std::vector<std::string> line;
		char* token = strtok(const_cast<char*>(boxes[boxIndexToCreate].c_str()), " ");
		while (token != nullptr) {
			line.push_back(token);
			token = strtok(nullptr, " ");
		}
		Box* box = new Box(line[0]);
		int numberOfItmes = stoi(line[1]);
		for (size_t i = 1; i <= numberOfItmes; i++) {
			box->insertItem(line[1 + i]);
		}
		int numberOfInnerBoxes = stoi(line[numberOfItmes + 2]);

		for (size_t i = 0; i < numberOfInnerBoxes; i++) {
			Box* innerBox = createBox(boxes, ++boxIndexToCreate);
			box->insertBox(innerBox);
		}
		return box;
	}
	
	void createBoxes(std::vector<std::string>& boxes, int numberOfBoxes) {
		for (int i = 0; i < numberOfBoxes; i++) {
			boxesList.push_back(createBox(boxes, i));
		}
	}

	void printBox(Box* box, int level) {
		for (size_t i = 0; i < level; i++) {
			std::cout << ("--");
		}
		std::cout << box->getName();
		if (box->getItems().size() > 0) {
			std::cout << " -> Items: ";
		}
		for (size_t i = 0; i < box->getItems().size(); i++) {
			std::cout << box->getItems()[i] << " ";
		}


		std::cout << std::endl;
		for (size_t i = 0; i < box->getBoxes().size(); i++) {
			printBox(box->getBoxes()[i], level + 1);
		}
	}
	
	void optimizeBoxesRec(Box* box, Box* parent) {
		for (int i = 0; i < box->getBoxes().size(); i++) {
			optimizeBoxesRec(box->getBoxes()[i], box);
		}


		if (box->isEmpty()) {
			if (parent != nullptr) {
				//std::remove - removes all elements that are equal to the box by putting them in the end of the vector and returns
				//an iterator to the first element that has been moved to the end of the vector
				//std::erase removes all elements in range 
				parent->getBoxes().erase(std::remove(parent->getBoxes().begin(), parent->getBoxes().end(), box), parent->getBoxes().end());
				delete box;
			}
			else {
				boxesList.erase(std::remove(boxesList.begin(), boxesList.end(), box), boxesList.end());
				delete box;
			}
		}
		else if (box->hasOnlyOneBox()) {
			if (parent != nullptr) {
				parent->getBoxes().erase(std::remove(parent->getBoxes().begin(), parent->getBoxes().end(), box), parent->getBoxes().end());
				parent->insertBoxFront(box->getBoxes()[0]);
				delete box;
			}
			else {
				boxesList.erase(std::remove(boxesList.begin(), boxesList.end(), box), boxesList.end());
				boxesList.insert(boxesList.begin(), box->getBoxes()[0]);
				delete box;
			}
		}
	}
	

	bool isPathValid(std::vector<std::string> path) {
		if (path.size() == 0) {
			return false;
		}
		return path[path.size() - 1].compare("Railstation") == 0;
	}

	int originalElementsOfPath(std::vector<std::string> path) {
		std::unordered_set<std::string> originalElements;

		for (auto& element : path) {
			originalElements.insert(element);
		}

		return originalElements.size();
	}

	std::vector<std::string> findBestPathInCityRec(std::vector<Vertice> vertices, std::string current, int time, std::vector<std::string> path, std::vector<std::string> bestPath) {

		Vertice cur = vertices[0];
		for (auto& vertice : vertices) {
			if (vertice.getName().compare(current) == 0) {
				cur = vertice;
				break;
			}
		}
		path.push_back(current);
		for (auto& edge : cur.getAdjacentsList()) {
			if (edge.second <= time) {
				auto p = findBestPathInCityRec(vertices, edge.first.getName(), time - edge.second, path, bestPath);
				if (isPathValid(p) && (originalElementsOfPath(p) > originalElementsOfPath(bestPath))) {
					bestPath = p;
				}
			}
		}

		if (isPathValid(path) && (originalElementsOfPath(path) > originalElementsOfPath(bestPath))) {
			bestPath = path;
		}
		return bestPath;

	}
	
	
	std::vector<std::string> findShortestPathRec(Node* current, std::map<std::string, bool> stops, std::vector<std::string> path) {
		//checks if we are in a cycle
		//for (size_t i = 0; i < path.size(); i++) {
		//	if (path[i].compare(current->data) == 0) {
		//		//return path with dummy text with size that is more than the cities size
		//		while (path.size() <= cities.getSize() + 1) {
		//			path.push_back("dummy");
		//		}
		//		return path;
		//	}
		//}
		path.push_back(current->data);
		if (current->next == nullptr) {
			for (auto& stop : stops) {
				if (stop.second == false) {
					//return path with dummy text with size that is more than the cities size
					while (path.size() <= cities.getSize() + 1) {
						path.push_back("dummy");
					}
					return path;
				}
			}
			return path;
		}

		if (stops.find(current->data) != stops.end()) {
			stops[current->data] = true;
		}

		if (current->skip != nullptr) {
			std::vector<std::string> path1 = findShortestPathRec(current->skip, stops, path);
			std::vector<std::string> path2 = findShortestPathRec(current->next, stops, path);
			if (path1.size() < path2.size()) {
				return path1;
			}
			else {
				return path2;
			}
		}
		else {
			return findShortestPathRec(current->next, stops, path);
		}

	}

public:

	void findBestPathInCity(const std::string& filePath) {
		std::ifstream inFile(filePath);
		if (!inFile.is_open()) {
			std::cout << "File not found!" << std::endl;
			return;
		}

		std::string line;
		std::vector<std::string> tokens;
		while (std::getline(inFile, line)) {
			//saw it on the internet https://cplusplus.com/reference/cstring/strtok/
			char* token = strtok(const_cast<char*>(line.c_str()), " ");
			while (token != nullptr) {
				tokens.push_back(token);
				token = strtok(nullptr, " ");
			}
			// --
		}

		inFile.close();

		std::vector<Vertice> vertices;
		std::unordered_set<std::string> addedVertices;
		for (size_t i = 2; i < tokens.size(); i += 3) {
			if (i == tokens.size() - 1) {
				break;
			}
			if (addedVertices.find(tokens[i]) == addedVertices.end()) {
				addedVertices.insert(tokens[i]);
				vertices.push_back(Vertice(tokens[i]));
			}
			if (addedVertices.find(tokens[i + 1]) == addedVertices.end()) {
				addedVertices.insert(tokens[i + 1]);
				vertices.push_back(Vertice(tokens[i + 1]));
			}

			Vertice* v1 = nullptr;
			Vertice* v2 = nullptr;
			for (auto& v : vertices) {
				if (v.getName().compare(tokens[i]) == 0) {
					v1 = &v;
				}
				if (v.getName().compare(tokens[i + 1]) == 0) {
					v2 = &v;
				}
			}
			//add both edges
			v1->addEdge(*v2, std::stoi(tokens[i + 2]));
			v2->addEdge(*v1, std::stoi(tokens[i + 2]));

		}
		int time = stoi(tokens[tokens.size() - 1]);


		auto path = findBestPathInCityRec(vertices, "Railstation", time, std::vector<std::string>(), std::vector<std::string>());
		if (path.size() == 0) {
			std::cout << "No path found!" << std::endl;
		}
		else {
			for (auto& element : path) {
				std::cout << element << " ";
			}
		}
	}

	void initializePath() {
		std::cout << "Enter number of cities: ";
		int n;
		std::cin >> n;
		std::cin.ignore();

		std::string city;
		for (size_t i = 0; i < n; i++) {
			std::cout << "Enter city: ";
			std::cin >> city;
			cities.add(city);
		}
		std::cout << "Enter number of skips: ";
		std::cin >> n;
		std::cin.ignore();
		for (size_t i = 0; i < n; i++) {
			std::string from;
			std::string to;
			std::cout << "Enter from: ";
			std::cin >> from;
			std::cout << "Enter to: ";
			std::cin >> to;
			cities.addSkip(from, to);
		}
	}

	void findShortestPath() {
		int n;
		std::cout << "Number of stops: ";
		std::cin >> n;
		std::cin.ignore();
		std::map<std::string, bool> stops;
		for (size_t i = 0; i < n; i++) {
			std::string stop;
			std::cout << "Enter stop: ";
			std::cin >> stop;
			stops[stop] = 0;
		}

		std::vector<std::string> shortestPath = findShortestPathRec(cities.getHead(), stops, std::vector<std::string>());

		for (size_t i = 0; i < shortestPath.size(); i++) {
			std::cout << shortestPath[i] << " ";
		}
	}

	void optimizeBoxes(const std::string& filePath) {
		std::ifstream inFile(filePath);
		if (!inFile.is_open()) {
			std::cout << "File not found!" << std::endl;
			return;
		}

		std::string line;
		std::vector<std::string> tokens;
		while (std::getline(inFile, line)) {
			tokens.push_back(line);
		}

		inFile.close();

		int numberOfBoxes = stoi(tokens[0]);
		tokens.erase(tokens.begin());
		createBoxes(tokens, numberOfBoxes);

		for (size_t i = 0; i < boxesList.size(); i++) {
			printBox(boxesList[i], 0);
		}
		std::cout << std::endl;


		for (size_t i = 0; i < boxesList.size(); i++) {
			optimizeBoxesRec(boxesList[i], nullptr);
		}

		for (size_t i = 0; i < boxesList.size(); i++) {
			printBox(boxesList[i], 0);
		}


		freeBoxes();
	}

};