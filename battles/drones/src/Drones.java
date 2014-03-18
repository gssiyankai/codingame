import java.util.*;

final class Player {

    static final Scanner SCANNER = new Scanner(System.in);
    static final int RADIUS2 = 100 * 100;

    static class Position {
        int x;
        int y;
    }

    static class Zone {
        Position position;
        int controller;
    }

    static class Drone {
        Position position;

        boolean isInZone(Zone zone) {
            return distance2(position, zone.position) <= RADIUS2;
        }
    }

    static class AiPlayer {
        int id;
        Drone[] drones;
    }

    static int P;
    static int I;
    static int D;
    static int Z;
    static Zone[] zones;
    static AiPlayer[] players;
    static Strategy strategy;
    static int[] targets;

    static class Strategy extends Thread {

        @Override
        public void run() {
            BitSet drones = moveableDrones();
            for (int i = drones.nextSetBit(0); i >= 0; i = drones.nextSetBit(i+1)) {
                int j = closestUncontrolledZone(myDrone(i));
                targets[i] = j;
            }
        }

    }

    static int distance2(Position a, Position b) {
        int deltaX = a.x - b.x;
        int deltaY = a.y - b.y;
        return deltaX * deltaX + deltaY * deltaY;
    }

    static Position nextPosition() {
        Position position = new Position();
        position.x = SCANNER.nextInt();
        position.y = SCANNER.nextInt();
        return position;
    }

    static Drone nextDrone() {
        Drone drone = new Drone();
        drone.position = nextPosition();
        return drone;
    }

    static Drone[] nextDrones() {
        Drone[] drones = new Drone[D];
        for(int i=0; i<D; ++i) {
            drones[i] = nextDrone();
        }
        return drones;
    }

    static void initializeZones() {
        zones = new Zone[Z];
        for(int i=0; i<Z; ++i) {
            Zone zone = new Zone();
            zone.position = nextPosition();
            zone.controller = -1;
            zones[i] = zone;
        }
    }

    static void initializePlayers() {
        players = new AiPlayer[P];
        for(int i=0; i<P; ++i) {
            AiPlayer player = new AiPlayer();
            player.id = i;
            players[i] = player;
        }
    }

    static void initialize() {
        P = SCANNER.nextInt();
        I = SCANNER.nextInt();
        D = SCANNER.nextInt();
        Z = SCANNER.nextInt();
        initializeZones();
        initializePlayers();

        targets = new int[D];
    }

    static void update() {
        for(int i=0; i<Z; ++i) {
            zones[i].controller = SCANNER.nextInt();
        }
        for(int i=0; i<P; ++i) {
            players[i].drones = nextDrones();
        }
    }

    static BitSet moveableDrones() {
        BitSet drones = new BitSet(D);
        drones.set(0, D);
        for(int i=0; i<Z; ++i) {
            Zone zone = zones[i];
            if(zone.controller==I) {
                BitSet myDrones = myDronesInZone(zone);
                int ennemyDrones = maxEnnemyDronesInZone(zone);
                for(int j=ennemyDrones; j<myDrones.cardinality(); ++j) {
                    myDrones.clear(myDrones.nextSetBit(0));
                }
                drones.andNot(myDrones);
            }
        }
        return drones;
    }

    static Drone ennemyDrone(int d, int p) {
        return players[p].drones[d];
    }

    static Drone myDrone(int d) {
        return players[I].drones[d];
    }

    static int closestUncontrolledZone(Drone drone) {
        int ret = 0;
        int min = Integer.MAX_VALUE;
        for(int i=0; i<Z; ++i) {
            Zone zone = zones[i];
            if(zone.controller!=I) {
                int dist = distance2(zone.position, drone.position);
                if(dist<min) {
                    ret = i;
                    min = dist;
                }
            }
        }
        return ret;
    }

    static BitSet myDronesInZone(Zone zone) {
        BitSet ret = new BitSet(D);
        for(int i=0; i<D; ++i) {
            if(myDrone(i).isInZone(zone)) {
                ret.set(i);
            }
        }
        return ret;
    }

    static int maxEnnemyDronesInZone(Zone zone) {
        int max = Integer.MIN_VALUE;
        for(int i=0; i<P; ++i) {
            if(i!=I) {
                int ennemies = ennemyDronesInZone(zone, i);
                if(ennemies>max) {
                    max = ennemies;
                }
            }
        }
        return max;
    }

    static int ennemyDronesInZone(Zone zone, int player) {
        int ret = 0;
        for(int i=0; i<D; ++i) {
            if(ennemyDrone(i, player).isInZone(zone)) {
                ++ret;
            }
        }
        return ret;
    }

    static void process() throws Exception {
        strategy = new Strategy();
        strategy.start();
        strategy.join(90);
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
            for(int target : targets) {
                Zone zone = zones[target];
                Position position = zone.position;
                System.out.print(position.x);
                System.out.print(" ");
                System.out.println(position.y);
            }
        }
    }

}
