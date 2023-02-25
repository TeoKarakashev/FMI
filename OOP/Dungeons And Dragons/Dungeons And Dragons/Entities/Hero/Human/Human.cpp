#include "Human.h"

Human::Human() : Hero(30, 20, 50) {
}

Human::Human(size_t power, size_t mana, size_t health, size_t level, const Inventory& inventory) :Hero(power, mana, health, level, inventory) {
}
