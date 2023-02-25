package bg.sofia.uni.fmi.mjt.cocktail.server.storage;

import bg.sofia.uni.fmi.mjt.cocktail.server.Cocktail;
import bg.sofia.uni.fmi.mjt.cocktail.server.storage.exceptions.CocktailAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.cocktail.server.storage.exceptions.CocktailNotFoundException;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class DefaultCocktailStorage implements CocktailStorage {

    private Set<Cocktail> cocktails;

    public DefaultCocktailStorage() {
        this.cocktails = new HashSet<>();
    }

    @Override
    public void createCocktail(Cocktail cocktail) throws CocktailAlreadyExistsException {
        if (cocktails.contains(cocktail)) {
            throw new CocktailAlreadyExistsException("Cocktail already exists");
        }
        cocktails.add(cocktail);
    }

    @Override
    public Collection<Cocktail> getCocktails() {
        return cocktails;
    }

    @Override
    public Collection<Cocktail> getCocktailsWithIngredient(String ingredientName) {
        return cocktails.stream()
            .filter(cocktail -> cocktail.ingredients().stream()
                .anyMatch(ingredient -> ingredient.name().equalsIgnoreCase(ingredientName)))
            .collect(Collectors.toSet());
    }

    @Override
    public Cocktail getCocktail(String name) throws CocktailNotFoundException {
        return cocktails.stream()
            .filter(cocktail -> cocktail.name().equalsIgnoreCase(name))
            .findFirst()
            .orElseThrow(() -> new CocktailNotFoundException("Cocktail not found"));
    }
}

