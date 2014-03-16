import java.util.*;
class Player {

    static final class Position {
        int x;
        int y;
    }

    static final class ZoneComparator implements Comparator<Integer> {

        final int player;
        final int drone;

        ZoneComparator(int player, int drone) {
            this.player = player;
            this.drone = drone;
        }

        @Override
        public int compare(Integer i1, Integer i2) {
            int[] distances = droneZoneDistances[player][drone];
            int distance1 = distances[i1];
            int distance2 = distances[i2];
            int weight1 = distance1;
            int weight2 = distance2;

            if(zoneControllers[i1]==-1 && numberOfDifferentEnnemiesInZone(i1)>1) {
                weight1 += distance2;
            }
            if(zoneControllers[i2]==-1 && numberOfDifferentEnnemiesInZone(i2)>1) {
                weight2 += distance1;
            }
            if(zoneControllers[i1]!=playerId) {
                weight1 += (maxEnnemiesPerPlayer(i1)-dronesInZone[i1][player]) * distance1;
            }
            if(zoneControllers[i2]!=playerId) {
                weight2 += (maxEnnemiesPerPlayer(i2)-dronesInZone[i1][player]) * distance2;
            }
            if(zoneControllers[i1]==playerId && maxEnnemiesPerPlayer(i1)+1<=dronesInZone[i1][player]) {
                weight1 += (dronesInZone[i1][player]-(maxEnnemiesPerPlayer(i1)+1)) * distance2;
            }
            if(zoneControllers[i2]==playerId && maxEnnemiesPerPlayer(i2)+1<=dronesInZone[i2][player]) {
                weight2 += (dronesInZone[i2][player]-(maxEnnemiesPerPlayer(i2)+1)) * distance1;
            }

            return Integer.compare(weight1, weight2);
        }

        int numberOfDifferentEnnemiesInZone(int zone) {
            int n = 0;
            for(int i=0; i<numberOfPlayers; ++i) {
                if(i!=player) {
                    n += dronesInZone[zone][i];
                }
            }
            return n;
        }

        int maxEnnemiesPerPlayer(int zone) {
            int max=0;
            for(int i=0; i<numberOfPlayers; ++i) {
                if(i!=player) {
                    int n = dronesInZone[zone][i];
                    if(n>max) {
                        max = n;
                    }
                }
            }
            return max;
        }

    }

    static final class MyThread extends Thread {
        @Override
        public void run() {
            while(true) {
                for(int i=0; i<numberOfDrones; ++i) {
                    Arrays.sort(droneSortedZones[playerId][i], zoneComparators[playerId][i]);
                }
            }
        }
    }

    static MyThread myThread;

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
    static int[][][] droneZoneDistances;
    static Integer[][][] droneSortedZones;
    static ZoneComparator[][] zoneComparators;

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
        droneZoneDistances = new int[numberOfPlayers][numberOfDrones][numberOfZones];
        zoneComparators = new ZoneComparator[numberOfPlayers][numberOfDrones];
        for(int i=0; i<numberOfPlayers; ++i) {
            for(int j=0; j<numberOfDrones; ++j) {
                zoneComparators[i][j] = new ZoneComparator(i,j);
            }
        }
        droneSortedZones = new Integer[numberOfPlayers][numberOfDrones][numberOfZones];
        for(int i=0; i<numberOfPlayers; ++i) {
            for(int j=0; j<numberOfDrones; ++j) {
                for(int k=0; k<numberOfZones; ++k) {
                    droneSortedZones[i][j][k] = k;
                }
            }
        }
        myThread = new MyThread();
        myThread.start();
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
        for(int i=0; i<numberOfPlayers; ++i) {
            for(int j=0; j<numberOfDrones; ++j) {
                for(int k=0; k<numberOfZones; ++k) {
                    droneZoneDistances[i][j][k] = distance2(dronePositions[i][j], zonePositions[k]);
                }
            }
        }
    }


    static void process() throws Exception {
        myThread.join(90);
        for(int i=0; i<numberOfDrones; ++i) {
            droneZoneTargets[i] = droneSortedZones[playerId][i][0];
        }
    }

    public static void main(String args[]) throws Exception {
        // Read init information from standard input, if any
        initialize();

        while (true) {
            // Read information from standard input
            update();

            // Compute logic here
            process();

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