#include "Warrior.h"

Warrior::Warrior() : Hero(40, 10, 50) {
}

Warrior::Warrior(size_t power, size_t mana, size_t health, size_t level, const Inventory& inventory) :Hero(power, mana, health, level, inventory) {
}
