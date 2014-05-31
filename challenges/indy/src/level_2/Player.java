package level_2;

import java.util.*;

class Player {

    public static enum ROTATION {
        LEFT, RIGHT
    }

    public static enum MOVE {
        UP, DOWN, LEFT, RIGHT, INVALID
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
        type = Math.abs(type);
        switch (type) {
            case 1:
                return MOVE.DOWN;
            case 2:
                if(previousMove==MOVE.LEFT || previousMove==MOVE.RIGHT) {
                    return previousMove;
                }
                break;
            case 3:
                if(previousMove==MOVE.DOWN) {
                    return MOVE.DOWN;
                }
                break;
            case 4:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.LEFT;
                } else if(previousMove==MOVE.LEFT) {
                    return MOVE.DOWN;
                }
                break;
            case 5:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.RIGHT;
                } else if(previousMove==MOVE.RIGHT) {
                    return MOVE.DOWN;
                }
                break;
            case 6:
                if (previousMove==MOVE.LEFT || previousMove==MOVE.RIGHT) {
                    return previousMove;
                }
                break;
            case 7:
                if (previousMove==MOVE.DOWN || previousMove==MOVE.LEFT) {
                    return MOVE.DOWN;
                }
                break;
            case 8:
                if (previousMove==MOVE.LEFT || previousMove==MOVE.RIGHT) {
                    return MOVE.DOWN;
                }
                break;
            case 9:
                if (previousMove==MOVE.DOWN || previousMove==MOVE.RIGHT) {
                    return MOVE.DOWN;
                }
                break;
            case 10:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.LEFT;
                }
                break;
            case 11:
                if (previousMove==MOVE.DOWN) {
                    return MOVE.RIGHT;
                }
                break;
            case 12:
                if (previousMove==MOVE.LEFT) {
                    return MOVE.DOWN;
                }
                break;
            case 13:
                if (previousMove==MOVE.RIGHT) {
                    return MOVE.DOWN;
                }
                break;
        }
        return MOVE.INVALID;
    }
    
    public static int[] nextPos(int x, int y, MOVE move) {
        int[] nextPos = new int[2];
        switch (move) {
            case UP:
                nextPos[0] = x;
                nextPos[1] = y-1;
                break;
            case DOWN:
                nextPos[0] = x;
                nextPos[1] = y+1;
                break;
            case LEFT:
                nextPos[0] = x-1;
                nextPos[1] = y;
                break;
            case RIGHT:
                nextPos[0] = x+1;
                nextPos[1] = y;
                break;
            default:
                throw new UnsupportedOperationException(x + " " + y + " " + move);
        }
        return nextPos;
    }
    
    public static int newType(int type, ROTATION rotation) {
        if(rotation==ROTATION.LEFT) {
            if(type==2) return 3;
            if(type==3) return 2;
            if(type==4) return 5;
            if(type==5) return 4;
            if(type==6) return 9;
            if(type==7) return 6;
            if(type==8) return 7;
            if(type==9) return 8;
            if(type==10) return 13;
            if(type==11) return 10;
            if(type==12) return 11;
            if(type==13) return 12;
        } else if (rotation==ROTATION.RIGHT) {
            if(type==2) return 3;
            if(type==3) return 2;
            if(type==4) return 5;
            if(type==5) return 4;
            if(type==6) return 7;
            if(type==7) return 8;
            if(type==8) return 9;
            if(type==9) return 6;
            if(type==10) return 11;
            if(type==11) return 12;
            if(type==12) return 13;
            if(type==13) return 10;
        }
        return type;
    }
    
    public static String nextAction(int[] nextPos, int[][] types, MOVE move) {
        int nextType = types[nextPos[1]][nextPos[0]];
        MOVE nextMove = nextMove(nextType, move);
        if (nextMove==MOVE.INVALID) {
            ROTATION rotation = ROTATION.LEFT;
            int newType = newType(nextType, rotation);
            types[nextPos[1]][nextPos[0]] = newType;
            if(nextMove(newType, move)==MOVE.INVALID) {
                rotation = ROTATION.RIGHT;
                newType = newType(nextType, rotation);
                types[nextPos[1]][nextPos[0]] = newType;
            }
            return "" + nextPos[0] + " " + nextPos[1] + " " + rotation;
        } else {
            return "WAIT";
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
        System.err.println("exit="+exit);

        while (true) {
            
            System.err.println("types="+Arrays.deepToString(types));
            
            // Read information from standard input
            int x = in.nextInt();
            int y = in.nextInt();
            String pos = in.next();
            int r = in.nextInt();

            System.err.println("x="+x);
            System.err.println("y="+y);
            System.err.println("pos="+pos);
            System.err.println("r="+r);

            // Compute logic here
            MOVE move = nextMove(types[y][x], previousMove(pos));
            int[] nextPos = nextPos(x, y, move);
            String action = nextAction(nextPos, types, move);

            // Write action to standard output
            System.out.println(action);
        }
    }
}