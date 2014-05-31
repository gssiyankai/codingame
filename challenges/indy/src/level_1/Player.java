package level_1;

import java.util.*;

class Player {

    public static enum MOVE {
        UP, DOWN, LEFT, RIGHT
    }

    public static MOVE previousMove(String pos) {
        if (pos.equals("TOP")) {
            return MOVE.DOWN;
        } else if (pos.equals("LEFT")) {
            return MOVE.RIGHT;
        } else if (pos.equals("RIGHT")) {
            return MOVE.LEFT;
        } else {
            throw new UnsupportedOperationException();
        }
    }

    public static MOVE nextMove(int type, MOVE previousMove) {
        switch (type) {
            case 1:
            case 3:
            case 7:
            case 8:
            case 9:
            case 12:
            case 13:
                return MOVE.DOWN;
            case 2:
                return previousMove;
            case 4:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.LEFT;
                } else if(previousMove==MOVE.LEFT) {
                    return MOVE.DOWN;
                }
            case 5:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.RIGHT;
                } else if(previousMove==MOVE.RIGHT) {
                    return MOVE.DOWN;
                }
            case 6:
                if (previousMove==MOVE.LEFT || previousMove==MOVE.RIGHT) {
                    return previousMove;
                }
            case 10:
                return MOVE.LEFT;
            case 11:
                return MOVE.RIGHT;
        }
        throw new UnsupportedOperationException();
    }

    public static String nextAction(int x, int y, MOVE move) {
        switch (move) {
            case UP:
                return "" + x + " " + (y-1);
            case DOWN:
                return "" + x + " " + (y+1);
            case LEFT:
                return "" + (x-1) + " " + y;
            case RIGHT:
                return "" + (x+1) + " " + y;
            default:
                throw new UnsupportedOperationException();
        }
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);

        // Read init information from standard input, if any
        int w = in.nextInt();
        int h = in.nextInt();

        int[][] types = new int[h][w];
        for(int i=0; i<h; ++i) {
            for(int j=0; j<w; ++j) {
                types[i][j] = in.nextInt();
            }
        }

        int exit = in.nextInt();

        System.err.println("w="+w);
        System.err.println("h="+h);
        System.err.println("types="+Arrays.deepToString(types));
        System.err.println("exit="+exit);

        while (true) {
            // Read information from standard input
            int x = in.nextInt();
            int y = in.nextInt();
            String pos = in.next();

            System.err.println("x="+x);
            System.err.println("y="+y);
            System.err.println("pos="+pos);

            // Compute logic here
            MOVE move = nextMove(types[y][x], previousMove(pos));
            String action = nextAction(x, y, move);

            // Write action to standard output
            System.out.println(action);
        }
    }
}