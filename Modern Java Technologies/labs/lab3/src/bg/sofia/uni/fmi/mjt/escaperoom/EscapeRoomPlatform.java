package bg.sofia.uni.fmi.mjt.escaperoom;

import bg.sofia.uni.fmi.mjt.escaperoom.exception.PlatformCapacityExceededException;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.RoomAlreadyExistsException;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.RoomNotFoundException;
import bg.sofia.uni.fmi.mjt.escaperoom.exception.TeamNotFoundException;
import bg.sofia.uni.fmi.mjt.escaperoom.room.EscapeRoom;
import bg.sofia.uni.fmi.mjt.escaperoom.room.Review;
import bg.sofia.uni.fmi.mjt.escaperoom.team.Team;

public class EscapeRoomPlatform implements EscapeRoomAdminAPI, EscapeRoomPortalAPI {

    private EscapeRoom[] rooms;
    private Team[] teams;
    private int roomsCount;
    private int teamsCount;
    private int maxCapacity;


    public EscapeRoomPlatform(Team[] teams, int maxCapacity) {
        this.teams = teams;
        this.maxCapacity = maxCapacity;
        this.rooms = new EscapeRoom[maxCapacity];
        this.teamsCount = teams.length;
        this.roomsCount = 0;
    }


    @Override
    public void addEscapeRoom(EscapeRoom room) throws RoomAlreadyExistsException {
        if (room == null) {
            throw new IllegalArgumentException();
        }
        if (roomsCount == maxCapacity) {
            throw new PlatformCapacityExceededException("The platform is full");
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(room.getName())) {
                throw new RoomAlreadyExistsException("Room already exists");
            }
        }
        rooms[roomsCount++] = room;
    }


    @Override
    public void removeEscapeRoom(String roomName) throws RoomNotFoundException {

        if (roomName == null || roomName.isBlank()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(roomName)) {
                for (int j = i; j < roomsCount - 1; j++) {
                    rooms[j] = rooms[j + 1];
                }
                roomsCount--;
                return;
            }
        }
        throw new RoomNotFoundException("Room not found");
    }

    @Override
    public EscapeRoom[] getAllEscapeRooms() {
        EscapeRoom[] allRooms = new EscapeRoom[roomsCount];
        for (int i = 0; i < roomsCount; i++) {
            allRooms[i] = rooms[i];
        }
        return allRooms;
    }

    @Override
    public void registerAchievement(String roomName, String teamName, int escapeTime) throws RoomNotFoundException, TeamNotFoundException {
        if (roomName == null || roomName.isBlank() || teamName == null || teamName.isBlank() || escapeTime <= 0) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(roomName)) {
                if (escapeTime > rooms[i].getMaxTimeToEscape()) {
                    throw new IllegalArgumentException();
                }
                for (int j = 0; j < teamsCount; j++) {
                    if (teams[j].getName().equalsIgnoreCase(teamName)) {
                        int pointsToAdd = rooms[i].getDifficulty().getRank();
                        if (escapeTime <= rooms[i].getMaxTimeToEscape() / 2) {
                            pointsToAdd += 2;
                        } else if (escapeTime <= rooms[i].getMaxTimeToEscape() * 0.75) {
                            pointsToAdd += 1;
                        }
                        teams[j].updateRating(pointsToAdd);
                        return;
                    }
                }
                throw new TeamNotFoundException("Team not found");
            }
        }
        throw new RoomNotFoundException("Room not found");
    }


    @Override
    public EscapeRoom getEscapeRoomByName(String roomName) throws RoomNotFoundException {
        if (roomName == null || roomName.isBlank()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(roomName)) {
                return rooms[i];
            }
        }
        throw new RoomNotFoundException("Room not found");
    }

    @Override
    public void reviewEscapeRoom(String roomName, Review review) throws RoomNotFoundException {
        if (roomName == null || roomName.isBlank()) {
            throw new IllegalArgumentException();
        }
        if (review == null) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(roomName)) {
                rooms[i].addReview(review);
                return;
            }
        }
        throw new RoomNotFoundException("Room not found");
    }

    @Override
    public Review[] getReviews(String roomName) throws RoomNotFoundException {
        if (roomName == null || roomName.isBlank()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < roomsCount; i++) {
            if (rooms[i].getName().equalsIgnoreCase(roomName)) {
                return rooms[i].getReviews();
            }
        }
        throw new RoomNotFoundException("Room not found");
    }

    @Override
    public Team getTopTeamByRating() {
        if (teamsCount == 0) {
            return null;
        }
        Team topTeam = teams[0];
        for (int i = 1; i < teamsCount; i++) {
            if (teams[i].getRating() > topTeam.getRating()) {
                topTeam = teams[i];
            }
        }
        return topTeam;
    }
}
