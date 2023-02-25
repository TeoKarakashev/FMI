package bg.sofia.uni.fmi.mjt.grading.simulator.grader;

import bg.sofia.uni.fmi.mjt.grading.simulator.Assistant;
import bg.sofia.uni.fmi.mjt.grading.simulator.assignment.Assignment;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;

public class CodePostGrader implements AdminGradingAPI {


    private List<Assistant> assistants;
    private Queue<Assignment> assignments;
    private int submittedAssignmentsCount;
    private boolean isFinalized;

    private void createAndStartAssistants(int numberOfAssistants) {
        assistants = new ArrayList<>(numberOfAssistants);
        for (int i = 1; i <= numberOfAssistants; i++) {
            Assistant assistant = new Assistant("as" + i, this);
            assistants.add(assistant);
            assistant.start();
        }
        isFinalized = false;
    }

    public CodePostGrader(int numberOfAssistants) {
        assignments = new java.util.concurrent.ConcurrentLinkedQueue<>();
        submittedAssignmentsCount = 0;
        createAndStartAssistants(numberOfAssistants);
    }

    @Override
    public synchronized Assignment getAssignment() {

        while (!assignments.isEmpty() || !isFinalized) {
            try {
                if (isFinalized) {
                    return assignments.poll();
                }

                if (assignments.isEmpty()) {
                    this.wait();
                }
                return assignments.poll();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }


    @Override
    public int getSubmittedAssignmentsCount() {
        return submittedAssignmentsCount;
    }

    @Override
    public void finalizeGrading() {
        synchronized (this) {
            isFinalized = true;
            this.notifyAll();
        }
    }

    @Override
    public List<Assistant> getAssistants() {
        return assistants;
    }


    @Override
    public void submitAssignment(Assignment assignment) {
        try {
            synchronized (this) {
                assignments.add(assignment);
                this.notifyAll();
                submittedAssignmentsCount++;
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
