package bg.sofia.uni.fmi.mjt.escaperoom.team;

import bg.sofia.uni.fmi.mjt.escaperoom.rating.Ratable;

public class Team implements Ratable {

    private TeamMember[] teamMembers;
    private String name;
    private double rating;


    private Team(String name, TeamMember[] members) {
        this.name = name;
        this.teamMembers = members;
    }


    public static Team of(String name, TeamMember[] members) {
        return new Team(name, members);
    }

    /**
     * Updates the team rating by adding the specified points to it.
     *
     * @param points the points to be added to the team rating.
     * @throws IllegalArgumentException if the points are negative.
     */
    public void updateRating(int points) throws IllegalArgumentException {
        if (points < 0) {
            throw new IllegalArgumentException("Points cannot be negative");
        }
        this.rating += points;
    }

    /**
     * Returns the team name.
     */
    public String getName() {
        return name;
    }

    @Override
    public double getRating() {
        return rating;
    }
}
