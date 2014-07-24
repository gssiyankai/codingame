import sys

# Read init information from standard input, if any
W, H = map(lambda x: int(x), input().split(' '))
J = int(input())
X0, Y0 = map(lambda x: int(x), input().split(' '))

print("W=%s" % W, "H=%s" % H, file=sys.stderr)
print("J=%s" % J, file=sys.stderr)
print("X0=%s" % X0, "Y0=%s" % Y0, file=sys.stderr)

X1 = X0
Y1 = Y0
X2 = X1
Y2 = Y1
XS = range(0, W)
YS = range(0,H)

bomb_x_found = False

while 1:
    # Read information from standard input
    code = input()
    print("code=%s" % code, file=sys.stderr)

    # Compute logic here
    if code == "UNKNOWN":
        X2 = XS[len(XS)//2]
    elif code == "WARMER":
        if not bomb_x_found:
            XS = [x for x in XS if abs(x-X2) < abs(x-X1)]
            X1 = X2
            X2 = XS[len(XS)//2]
        else:
            YS = [y for y in YS if abs(y-Y2) < abs(y-Y1)]
            Y1 = Y2
            Y2 = YS[len(YS)//2]
    elif code == "COLDER":
        if not bomb_x_found:
            XS = [x for x in XS if abs(x-X1) < abs(x-X2)]
            X1 = X2
            X2 = XS[len(XS)//2]
        else:
            YS = [y for y in YS if abs(y-Y1) < abs(y-Y2)]
            Y1 = Y2
            Y2 = YS[len(YS)//2]
    elif code == "SAME":
        if not bomb_x_found:
            bomb_x_found = True
            if len(XS) > 1:
                XS = [x for x in XS if x != X2]
            Y2 = YS[len(YS)//2]
        else:
            if len(YS) > 1:
                YS = [y for y in YS if y != Y2]
                Y2 = YS[len(YS)//2]

    # Write action to standard output
    print("%s %s" % (X2,Y2))