package bg.sofia.uni.fmi.mjt.myfitnesspal.diary;

import bg.sofia.uni.fmi.mjt.myfitnesspal.exception.UnknownFoodException;
import bg.sofia.uni.fmi.mjt.myfitnesspal.nutrition.NutritionInfo;
import bg.sofia.uni.fmi.mjt.myfitnesspal.nutrition.NutritionInfoAPI;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class DailyFoodDiaryTest {
    @Mock
    private NutritionInfoAPI nutritionInfoAPIMock ;
    @InjectMocks
    private DailyFoodDiary dailyFoodDiary;

    @Test
    public void testAddFoodEntryShouldThrowWhenMealIsNull() {
        assertThrows(IllegalArgumentException.class,
            () -> dailyFoodDiary.addFood(null, "food", 1),
            "Meal cannot be null");
    }

    @Test
    public void testAddFoodEntryShouldThrowWhenFoodNameNullOrBlank() {
        assertThrows(IllegalArgumentException.class,
            () -> dailyFoodDiary.addFood(Meal.BREAKFAST, null, 1),
            "Food name cannot be null");
        assertThrows(IllegalArgumentException.class,
            () -> dailyFoodDiary.addFood(Meal.BREAKFAST, "  ", 1),
            "Food name cannot be blank");
    }

    @Test
    public void testAddFoodEntryShouldThrowWhenServingSizeIsNegative() {
        assertThrows(IllegalArgumentException.class,
            () -> dailyFoodDiary.addFood(Meal.BREAKFAST, "food", -1),
            "Food serving size cannot be negative");
    }

    @Test
    public void testAddFoodShouldWorkCorrectly() throws UnknownFoodException {

        when(nutritionInfoAPIMock.getNutritionInfo("pizza")).thenReturn(new NutritionInfo(85, 10, 5));


        NutritionInfo nutritionInfo = new NutritionInfo(85, 10, 5);
        FoodEntry pizza = new FoodEntry("pizza", 100, nutritionInfo);

        Assertions.assertEquals(pizza,
            dailyFoodDiary.addFood(Meal.LUNCH, "pizza", 100),
            "Food entry should be added correctly");
        verify(nutritionInfoAPIMock).getNutritionInfo("pizza");
        verify(nutritionInfoAPIMock, times(1)).getNutritionInfo("pizza");
    }

    @Test
    public void testGetAllFoodEntriesWorksWithEmptyDiary() {
        assertEquals(0, dailyFoodDiary.getAllFoodEntries().size(), "Diary should be empty");
    }

    @Test
    public void testGetAllFoodEntriesWorksCorrectly() throws UnknownFoodException {
        NutritionInfo nutritionInfo = new NutritionInfo(85, 10, 5);
        FoodEntry pizza = new FoodEntry("pizza", 100, nutritionInfo);
        FoodEntry salad = new FoodEntry("salad", 100, nutritionInfo);
        FoodEntry soup = new FoodEntry("soup", 100, nutritionInfo);

        when(nutritionInfoAPIMock.getNutritionInfo("pizza")).thenReturn(new NutritionInfo(85, 10, 5));
        when(nutritionInfoAPIMock.getNutritionInfo("salad")).thenReturn(new NutritionInfo(85, 10, 5));
        when(nutritionInfoAPIMock.getNutritionInfo("soup")).thenReturn(new NutritionInfo(85, 10, 5));

        dailyFoodDiary.addFood(Meal.LUNCH, "pizza", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "salad", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "soup", 100);

        assertEquals(3, dailyFoodDiary.getAllFoodEntries().size(), "Diary should contain 3 food entries");
        assertTrue(dailyFoodDiary.getAllFoodEntries().contains(pizza), "Diary should contain pizza");
        assertTrue(dailyFoodDiary.getAllFoodEntries().contains(salad), "Diary should contain salad");
        assertTrue(dailyFoodDiary.getAllFoodEntries().contains(soup), "Diary should contain soup");
        assertThrows(UnsupportedOperationException.class,
            () -> dailyFoodDiary.getAllFoodEntries().add(pizza),
            "Diary should be immutable");
    }

    @Test
    public void testGetAllFoodEntriesByProteinContentShouldWorkCorrectly() throws UnknownFoodException {
        NutritionInfo nutritionInfoPizza = new NutritionInfo(85, 10, 5);
        NutritionInfo nutritionInfoSalad = new NutritionInfo(50, 10, 40);
        NutritionInfo nutritionInfoSoup = new NutritionInfo(50, 30, 20);

        FoodEntry pizza = new FoodEntry("pizza", 100, nutritionInfoPizza);
        FoodEntry salad = new FoodEntry("salad", 100, nutritionInfoSalad);
        FoodEntry soup = new FoodEntry("soup", 100, nutritionInfoSoup);

        when(nutritionInfoAPIMock.getNutritionInfo("pizza")).thenReturn(new NutritionInfo(85, 10, 5));
        when(nutritionInfoAPIMock.getNutritionInfo("salad")).thenReturn(new NutritionInfo(50, 10, 40));
        when(nutritionInfoAPIMock.getNutritionInfo("soup")).thenReturn(new NutritionInfo(50, 30, 20));

        dailyFoodDiary.addFood(Meal.LUNCH, "pizza", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "salad", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "soup", 100);

        assertEquals(3, dailyFoodDiary.getAllFoodEntriesByProteinContent().size(), "Diary should contain 3 food entries");
        assertEquals(dailyFoodDiary.getAllFoodEntriesByProteinContent().get(0), pizza,
            "Diary should be sorted by protein content");
        assertEquals(dailyFoodDiary.getAllFoodEntriesByProteinContent().get(1), soup,
            "Diary should be sorted by protein content");
        assertEquals(dailyFoodDiary.getAllFoodEntriesByProteinContent().get(2), salad,
            "Diary should be sorted by protein content");
        assertThrows(UnsupportedOperationException.class,
            () -> dailyFoodDiary.getAllFoodEntriesByProteinContent().add(pizza),
            "Diary should be immutable");
    }

    @Test
    public void testGetDailyCaloriesIntakeShouldWorkCorrectly() throws UnknownFoodException {
        NutritionInfo nutritionInfoPizza = new NutritionInfo(85, 10, 5);
        NutritionInfo nutritionInfoSalad = new NutritionInfo(50, 10, 40);
        NutritionInfo nutritionInfoSoup = new NutritionInfo(50, 30, 20);

        FoodEntry pizza = new FoodEntry("pizza", 100, nutritionInfoPizza);
        FoodEntry salad = new FoodEntry("salad", 100, nutritionInfoSalad);
        FoodEntry soup = new FoodEntry("soup", 100, nutritionInfoSoup);

        when(nutritionInfoAPIMock.getNutritionInfo("pizza")).thenReturn(new NutritionInfo(85, 10, 5));
        when(nutritionInfoAPIMock.getNutritionInfo("salad")).thenReturn(new NutritionInfo(50, 10, 40));
        when(nutritionInfoAPIMock.getNutritionInfo("soup")).thenReturn(new NutritionInfo(50, 30, 20));

        dailyFoodDiary.addFood(Meal.LUNCH, "pizza", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "salad", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "soup", 100);

        assertEquals(1450, dailyFoodDiary.getDailyCaloriesIntake(), "GetDailyCaloriesIntake should work correctly");
    }

    @Test
    public void testGetDailyCaloriesIntakeShouldWorkCorrectlyWithEmptyDiary() {
        assertEquals(0, dailyFoodDiary.getDailyCaloriesIntake(), "Diary should work correctly with 0 food entries");
    }



    @Test
    public void testGetDailyCaloriesIntakePerMealShouldThrowWithInvalidMeal() {
        assertThrows(IllegalArgumentException.class,
            () -> dailyFoodDiary.getDailyCaloriesIntakePerMeal(null),
            "Diary should throw with invalid meal");
    }

    @Test
    public void testGetDailyCaloriesIntakePerMealShouldWorkCorrectly() throws UnknownFoodException {
        NutritionInfo nutritionInfoPizza = new NutritionInfo(85, 10, 5);
        NutritionInfo nutritionInfoSalad = new NutritionInfo(50, 10, 40);
        NutritionInfo nutritionInfoSoup = new NutritionInfo(50, 30, 20);

        FoodEntry pizza = new FoodEntry("pizza", 100, nutritionInfoPizza);
        FoodEntry salad = new FoodEntry("salad", 100, nutritionInfoSalad);
        FoodEntry soup = new FoodEntry("soup", 100, nutritionInfoSoup);

        when(nutritionInfoAPIMock.getNutritionInfo("pizza")).thenReturn(new NutritionInfo(85, 10, 5));
        when(nutritionInfoAPIMock.getNutritionInfo("salad")).thenReturn(new NutritionInfo(50, 10, 40));
        when(nutritionInfoAPIMock.getNutritionInfo("soup")).thenReturn(new NutritionInfo(50, 30, 20));

        dailyFoodDiary.addFood(Meal.LUNCH, "pizza", 100);
        dailyFoodDiary.addFood(Meal.LUNCH, "salad", 100);
        dailyFoodDiary.addFood(Meal.SNACKS, "soup", 100);

        assertEquals(0, dailyFoodDiary.getDailyCaloriesIntakePerMeal(Meal.BREAKFAST), "Diary should work correctly with no breakfast");
        assertEquals(900, dailyFoodDiary.getDailyCaloriesIntakePerMeal(Meal.LUNCH), "Diary should work correctly with lunch");
        assertEquals(550, dailyFoodDiary.getDailyCaloriesIntakePerMeal(Meal.SNACKS), "Diary should work correctly with snacks");
    }

}
