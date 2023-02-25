package bg.sofia.uni.fmi.mjt.grading.simulator;

import bg.sofia.uni.fmi.mjt.grading.simulator.assignment.Assignment;
import bg.sofia.uni.fmi.mjt.grading.simulator.grader.AdminGradingAPI;

public class Assistant extends Thread {

    private String name;
    private AdminGradingAPI adminGradingAPI;
    private int numberOfGradedAssignments;


    public Assistant(String name, AdminGradingAPI grader) {
        this.name = name;
        this.adminGradingAPI = grader;
        this.numberOfGradedAssignments = 0;
    }

    @Override
    public void run() {
        synchronized (adminGradingAPI) {
            Assignment assignment = adminGradingAPI.getAssignment();

            while (assignment != null) {
                try {
                    Thread.sleep(assignment.type().getGradingTime());
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                numberOfGradedAssignments++;
                assignment = adminGradingAPI.getAssignment();
            }
        }
    }


    public int getNumberOfGradedAssignments() {
        return numberOfGradedAssignments;
    }

}