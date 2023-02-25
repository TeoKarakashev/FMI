#include "Wizard.h"

Wizzard::Wizzard() : Hero(10, 40, 50) {
}

Wizzard::Wizzard(size_t power, size_t mana, size_t health, size_t level, const Inventory& inventory) : Hero(power, mana, health, level, inventory) {
}
