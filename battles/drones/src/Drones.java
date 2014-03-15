import java.util.*;

class Player {

    static class Position {
        int x;
        int y;
    }

    static Scanner scanner = new Scanner(System.in);
    static int numberOfPlayers;
    static int playerId;
    static int numberOfDrones;
    static int numberOfZones;
    static Position[] zonePositions;
    static int[] zoneControllers;
    static Position[][] dronePositions;
    static int[] droneMoves;

    static void initialize() {
        numberOfPlayers = scanner.nextInt();
        playerId = scanner.nextInt();
        numberOfDrones = scanner.nextInt();
        numberOfZones = scanner.nextInt();
        zonePositions = new Position[numberOfZones];
        for(int i=0; i<numberOfZones; ++i) {
            Position position = new Position();
            position.x = scanner.nextInt();
            position.y = scanner.nextInt();
            zonePositions[i] = position;
        }
        zoneControllers = new int[numberOfZones];
        dronePositions = new Position[numberOfPlayers][numberOfDrones];
        for(int i=0; i<numberOfPlayers; ++i) {
            for(int j=0; j<numberOfDrones; ++j) {
                dronePositions[i][j] = new Position();
            }
        }
        droneMoves = new int[numberOfDrones];
    }

    static void update() {
        for(int i=0; i<numberOfZones; ++i) {
            zoneControllers[i] = scanner.nextInt();
        }
        for(int i=0; i<numberOfPlayers; ++i) {
            for(int j=0; j<numberOfDrones; ++j) {
                dronePositions[i][j].x = scanner.nextInt();
                dronePositions[i][j].y = scanner.nextInt();
            }
        }
    }

    static int findClosestZone(Position drone) {
        int zone = 0;
        int min = Integer.MAX_VALUE;
        for(int i=0; i<numberOfZones; ++i) {
            Position zonePosition = zonePositions[i];
            int delta_x = drone.x - zonePosition.x;
            int delta_y = drone.y - zonePosition.y;
            int distance = delta_x*delta_x + delta_y*delta_y;
            if(distance<min) {
                zone = i;
                min = distance;
            }
            if(min==0) {
                break;
            }
        }
        return zone;
    }

    static void compute() {
        for(int i=0; i<numberOfDrones; ++i) {
            droneMoves[i] = findClosestZone(dronePositions[playerId][i]);
        }
        for(int i=0; i<numberOfDrones; ++i) {
            int droneMove = droneMoves[i];
            if(dronePositions[playerId][i]==zonePositions[droneMove] && zoneControllers[droneMove]!=playerId) {
                
            }
        }
    }

    public static void main(String args[]) {
        // Read init information from standard input, if any
        initialize();

        while (true) {
            // Read information from standard input
            update();

            // Compute logic here
            compute();

            // Write action to standard output
            for(int droneMove : droneMoves) {
                Position position = zonePositions[droneMove];
                System.out.print(position.x);
                System.out.print(" ");
                System.out.println(position.y);
            }
        }
    }
}