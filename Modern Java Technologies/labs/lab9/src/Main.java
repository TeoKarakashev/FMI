import bg.sofia.uni.fmi.mjt.grading.simulator.Assistant;
import bg.sofia.uni.fmi.mjt.grading.simulator.Student;
import bg.sofia.uni.fmi.mjt.grading.simulator.grader.CodePostGrader;

import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        CodePostGrader grader = new CodePostGrader(5);

        List<Thread> s = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
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
        System.out.println(grader.getSubmittedAssignmentsCount());
        grader.getAssistants().forEach(a -> System.out.println(a.getName() + " " + a.getNumberOfGradedAssignments()));
    }
}