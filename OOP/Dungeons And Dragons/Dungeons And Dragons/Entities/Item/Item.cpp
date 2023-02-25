#include "Item.h"


Item::Item(const String& name, size_t percent) : name(name), percentIncrease(percent) {
}

const String& Item::getName() const {
    return name;
}

size_t Item::getPercentIncrease() const {
    return percentIncrease;
}

//size_t Item::getLevel() const {
//    return level;
//}

void Item::setName(const String& name) {
    this->name = name;
}

void Item::setPercent(size_t percent){
    this->percentIncrease = percent;
}

//void Item::setLevel(unsigned level){
//    this->level = level;
//}
