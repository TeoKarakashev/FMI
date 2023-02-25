#pragma once

class Monster {
	size_t power;
	size_t mana;
	size_t maxHealth;
	size_t currentHealth;
	size_t shield;
public:

	Monster();
	Monster(size_t power, size_t mana, size_t maxHealth, size_t currentHealth, size_t shield);

	void setPower(size_t power);
	void setMana(size_t mana);
	void setHealth(size_t health);
	void setCurrentHealth(size_t health);
	void setShield(size_t shield);

	size_t getPower() const;
	size_t getMana() const;
	size_t getHealth() const;
	size_t getCurrentHealth() const;
	size_t getShield() const;
};
