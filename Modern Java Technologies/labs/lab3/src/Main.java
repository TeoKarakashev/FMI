import bg.sofia.uni.fmi.mjt.escaperoom.EscapeRoomPlatform;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.PlatformCapacityExceededException;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.RoomAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.RoomNotFoundException;
import bg.sofia.uni.fmi.mjt.escaperoom.room.Difficulty;
import bg.sofia.uni.fmi.mjt.escaperoom.room.EscapeRoom;
import bg.sofia.uni.fmi.mjt.escaperoom.room.Review;
import bg.sofia.uni.fmi.mjt.escaperoom.room.Theme;
import bg.sofia.uni.fmi.mjt.escaperoom.team.Team;
import bg.sofia.uni.fmi.mjt.escaperoom.team.TeamMember;

import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) throws RoomAlreadyExistsException, PlatformCapacityExceededException, RoomNotFoundException {
        TeamMember[] members = new TeamMember[3];
        members[0] = new TeamMember("Ivan", LocalDateTime.of(1999, 1, 1, 1, 1));
        members[1] = new TeamMember("Pesho", LocalDateTime.of(1999, 1, 1, 1, 1));
        members[2] = new TeamMember("Gosho", LocalDateTime.of(1999, 1, 1, 1, 1));
        Team team = Team.of("Team", members);
        Team[] teams = new Team[1];
        EscapeRoomPlatform platform = new EscapeRoomPlatform(teams, 5);
        platform.addEscapeRoom(new EscapeRoom("Room", Theme.MYSTERY, Difficulty.MEDIUM, 15, 10, 5));
        platform.addEscapeRoom(new EscapeRoom("Room2", Theme.MYSTERY, Difficulty.MEDIUM, 15, 10, 5));
        platform.addEscapeRoom(new EscapeRoom("Room3", Theme.MYSTERY, Difficulty.MEDIUM, 15, 10, 5));
        platform.addEscapeRoom(new EscapeRoom("Room4", Theme.MYSTERY, Difficulty.MEDIUM, 15, 10, 5));
        platform.addEscapeRoom(new EscapeRoom("Room5", Theme.MYSTERY, Difficulty.MEDIUM, 15, 10, 5));
        platform.getEscapeRoomByName("Room").addReview(new Review(5, "good"));
        platform.getEscapeRoomByName("Room").addReview(new Review(5, "staa"));
        platform.getEscapeRoomByName("Room2").addReview(new Review(5, "good"));
        platform.getEscapeRoomByName("Room3").addReview(new Review(5, "good"));
        platform.getEscapeRoomByName("Room4").addReview(new Review(5, "good"));
        platform.getEscapeRoomByName("Room5").addReview(new Review(5, "good"));
        platform.getEscapeRoomByName("Room").addReview(new Review(7, "chak"));




    }

}