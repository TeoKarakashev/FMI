package bg.sofia.uni.fmi.mjt.smartfridge;

import bg.sofia.uni.fmi.mjt.smartfridge.exception.FridgeCapacityExceededException;
import bg.sofia.uni.fmi.mjt.smartfridge.exception.InsufficientQuantityException;
import bg.sofia.uni.fmi.mjt.smartfridge.ingredient.DefaultIngredient;
import bg.sofia.uni.fmi.mjt.smartfridge.ingredient.Ingredient;
import bg.sofia.uni.fmi.mjt.smartfridge.recipe.Recipe;
import bg.sofia.uni.fmi.mjt.smartfridge.storable.Storable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class SmartFridge implements SmartFridgeAPI {

    private int totalCapacity;
    private int currentCapacity;
    private List<Ingredient<? extends Storable>> ingredients;

    public SmartFridge(int totalCapacity) {
        this.totalCapacity = totalCapacity;
        currentCapacity = 0;
        ingredients = new ArrayList<>();
    }

    @Override
    public <E extends Storable> void store(E item, int quantity) throws FridgeCapacityExceededException {
        if (item == null || quantity <= 0) {
            throw new IllegalArgumentException();
        }
        if (currentCapacity + quantity > totalCapacity) {
            throw new FridgeCapacityExceededException(
                "There is no free space in the fridge to accommodate the item(s).");
        }
        currentCapacity += quantity;
        ingredients.add(new DefaultIngredient<>(item, quantity));
    }

    @Override
    public List<? extends Storable> retrieve(String itemName) {
        if (itemName == null || itemName.isBlank()) {
            throw new IllegalArgumentException();
        }
        List<Storable> retrievedItems = new ArrayList<>();
        Iterator<Ingredient<? extends Storable>> iterator = ingredients.iterator();
        while (iterator.hasNext()) {
            Ingredient<? extends Storable> ingredient = iterator.next();
            if (ingredient.item().getName().equals(itemName)) {
                for (int i = 0; i < ingredient.quantity(); i++) {
                    retrievedItems.add(ingredient.item());
                }
                currentCapacity -= ingredient.quantity();
                iterator.remove();
            }
        }
        return retrievedItems;

    }

    @Override
    public List<? extends Storable> retrieve(String itemName, int quantity) throws InsufficientQuantityException {
        boolean found = false;
        if (itemName == null || itemName.isBlank() || quantity <= 0) {
            throw new IllegalArgumentException();
        }
        List<Storable> retrievedItems = new ArrayList<>();
        int sumQuantity = 0;
        for (Ingredient<? extends Storable> ingredient : ingredients) {
            if (ingredient.item().getName().equals(itemName)) {
                found = true;
                sumQuantity += ingredient.quantity();
            }
        }
        if (!found) {
            throw new InsufficientQuantityException("Item was not found");
        }

        if (sumQuantity < quantity) {
            throw new InsufficientQuantityException("There is not enough quantity of the item in the fridge.");
        }

        Iterator<Ingredient<? extends Storable>> iterator = ingredients.iterator();
        while (iterator.hasNext()) {
            Ingredient<? extends Storable> ingredient = iterator.next();
            if (ingredient.item().getName().equals(itemName)) {
                int tempQuantity = quantity;
                for (int i = 0; i < ingredient.quantity(); i++) {
                    //ToDo make copy of the list with removed items
                    retrievedItems.add(ingredient.item());
                    quantity--;
                    currentCapacity--;
                    if (quantity == 0) {
                        if (ingredient.quantity() - tempQuantity == 0) {
                            iterator.remove();
                        } else {
                            iterator.remove();
                            ingredients.add(
                                new DefaultIngredient<>(ingredient.item(), ingredient.quantity() - tempQuantity));
                        }
                        return retrievedItems;
                    }
                }
                iterator.remove();
            }
        }
        return retrievedItems;
    }

    @Override
    public int getQuantityOfItem(String itemName) {
        if (itemName == null || itemName.isBlank()) {
            throw new IllegalArgumentException();
        }
        int quantity = 0;
        for (Ingredient<? extends Storable> ingredient : ingredients) {
            if (ingredient.item().getName().equals(itemName)) {
                quantity += ingredient.quantity();
            }
        }
        return quantity;
    }

    @Override
    public Iterator<Ingredient<? extends Storable>> getMissingIngredientsFromRecipe(Recipe recipe) {
        if (recipe == null) {
            throw new IllegalArgumentException();
        }
        List<Ingredient<? extends Storable>> missingIngredients = new ArrayList<>();
        missingIngredients.addAll(recipe.getIngredients());
        for (Ingredient<? extends Storable> fridgeIngredient : ingredients) {
            if (!fridgeIngredient.item().isExpired()) {
                for (Ingredient<? extends Storable> ingredient : missingIngredients) {
                    if (ingredient.item().getName().equals(fridgeIngredient.item().getName())) {
                        if (ingredient.quantity() <= fridgeIngredient.quantity()) {
                            missingIngredients.remove(ingredient);
                        } else {
                            missingIngredients.set(missingIngredients.indexOf(ingredient),
                                new DefaultIngredient<>(ingredient.item(),
                                    ingredient.quantity() - fridgeIngredient.quantity()));
                        }
                    }
                }

            }
        }

        return missingIngredients.iterator();
    }

    @Override
    public List<? extends Storable> removeExpired() {
        List<Storable> retrievedItems = new ArrayList<>();
        Iterator<Ingredient<? extends Storable>> iterator = ingredients.iterator();

        while (iterator.hasNext()) {
            Ingredient<? extends Storable> ingredient = iterator.next();
            if (ingredient.item().isExpired()) {
                for (int i = 0; i < ingredient.quantity(); i++) {
                    retrievedItems.add(ingredient.item());
                }
                currentCapacity -= ingredient.quantity();
                iterator.remove();
            }
        }
        return retrievedItems;
    }
}
