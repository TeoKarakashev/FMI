public class DataCenter {
    public static int getCommunicatingServersCount(int[][] map) {

        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[i].length; j++) {
                if (map[i][j] > 0) {
                    for (int k = i + 1; k < map.length; k++) {
                        if (map[k][j] > 0) {
                            map[i][j] = 2;
                            map[k][j] = 2;
                        }
                    }
                    for (int k = j + 1; k < map[i].length; k++) {
                        if (map[i][k] > 0) {
                            map[i][j] = 2;
                            map[i][k] = 2;
                        }
                    }
                }
            }
        }
        int count = 0;
        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[i].length; j++) {
                if (map[i][j] == 2) {
                    count++;
                }
            }
        }
        return count;
    }
}
