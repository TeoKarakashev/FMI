package bg.sofia.uni.fmi.mjt.grading.simulator;

import bg.sofia.uni.fmi.mjt.grading.simulator.assignment.Assignment;
import bg.sofia.uni.fmi.mjt.grading.simulator.assignment.AssignmentType;
import bg.sofia.uni.fmi.mjt.grading.simulator.grader.StudentGradingAPI;


public class Student implements Runnable {
    public static final int MAX_SLEEP_TIME = 1000;
    private int fn;
    private String name;
    private StudentGradingAPI grader;

    public Student(int fn, String name, StudentGradingAPI studentGradingAPI) {
        this.fn = fn;
        this.name = name;
        this.grader = studentGradingAPI;
    }

    @Override
    public void run() {
        AssignmentType type = AssignmentType.values()[(int) (Math.random() * AssignmentType.values().length)];
        try {
            Thread.sleep((long) (Math.random() * MAX_SLEEP_TIME));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        Assignment assignment = new Assignment(fn, name, type);
        grader.submitAssignment(assignment);
    }

    public int getFn() {
        return fn;
    }

    public String getName() {
        return name;
    }

    public StudentGradingAPI getGrader() {
        return grader;
    }

}