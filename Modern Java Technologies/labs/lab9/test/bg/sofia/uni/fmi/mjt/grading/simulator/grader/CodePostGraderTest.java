package bg.sofia.uni.fmi.mjt.grading.simulator.grader;

import bg.sofia.uni.fmi.mjt.grading.simulator.Assistant;
import bg.sofia.uni.fmi.mjt.grading.simulator.Student;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CodePostGraderTest {

    @Test
    public void testCodePostGraderShouldReturnTheCorrectAmountOfGradedSubmissions() {
        CodePostGrader grader = new CodePostGrader(5);

        List<Thread> s = new ArrayList<>();
        for (int i = 0; i < 30; i++) {
            Thread student = new Thread(new Student(i, "s" + i, grader));
            s.add(student);
            student.start();
        }

        for (Thread student : s) {
            try {
                student.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        grader.finalizeGrading();
        assertEquals(30, grader.getSubmittedAssignmentsCount(),
            "The number of graded submissions is not read correctly");
        assertEquals(30, grader.getAssistants().stream().mapToInt(Assistant::getNumberOfGradedAssignments).sum(),
            "There was a problem with the grading of the submissions");
    }
}
