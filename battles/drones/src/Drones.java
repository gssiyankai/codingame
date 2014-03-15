import java.util.*;

class Player {

    static class Position {
        int x;
        int y;
    }

    static final int RADIUS = 100*100;
    static Scanner scanner = new Scanner(System.in);
    static int numberOfPlayers;
    static int playerId;
    static int numberOfDrones;
    static int numberOfZones;
    static Position[] zonePositions;
    static int[] zoneControllers;
    static Position[][] dronePositions;
    static int[] droneZoneTargets;
    static int[][] dronesInZone;

    static int distance2(Position a, Position b) {
        int delta_x = a.x - b.x;
        int delta_y = a.y - b.y;
        return delta_x*delta_x + delta_y*delta_y;
    }

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
        droneZoneTargets = new int[numberOfDrones];
        dronesInZone = new int[numberOfZones][numberOfPlayers];
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
        for(int i=0; i<numberOfZones; ++i) {
            Position zone = zonePositions[i];
            for(int j=0; j<numberOfPlayers; ++j) {
                int drones = 0;
                for(int k=0; k<numberOfDrones; ++k) {
                    Position drone = dronePositions[j][k];
                    if(distance2(drone, zone)<=RADIUS) {
                        ++drones;
                    }
                }
                dronesInZone[i][j] = drones;
            }
        }
    }

    static int findClosestZone(Position drone) {
        int zone = 0;
        int min = Integer.MAX_VALUE;
        for(int i=0; i<numberOfZones; ++i) {
            Position zonePosition = zonePositions[i];
            int distance = distance2(drone, zonePosition);
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
            droneZoneTargets[i] = findClosestZone(dronePositions[playerId][i]);
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
            for(int target : droneZoneTargets) {
                Position zone = zonePositions[target];
                System.out.print(zone.x);
                System.out.print(" ");
                System.out.println(zone.y);
            }
        }
    }
}