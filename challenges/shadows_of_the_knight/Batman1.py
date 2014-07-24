import sys

# Read init information from standard input, if any
W, H = map(lambda x: int(x), input().split(' '))
J = int(input())
X0, Y0 = map(lambda x: int(x), input().split(' '))

print("W=%s" % W, "H=%s" % H, file=sys.stderr)
print("J=%s" % J, file=sys.stderr)
print("X0=%s" % X0, "Y0=%s" % Y0, file=sys.stderr)

X = X0
Y = Y0
XS = range(0, W)
YS = range(0,H)

while 1:
    # Read information from standard input
    direction = input()
    print("direction=%s" % direction, file=sys.stderr)

    # Compute logic here
    if 'R' in direction:
        XS = [x for x in XS if x>X]
    if 'L' in direction:
        XS = [x for x in XS if x<X]
    if 'D' in direction:
        YS = [y for y in YS if y>Y]
    if 'U' in direction:
        YS = [y for y in YS if y<Y]

    X = XS[len(XS)//2]
    Y = YS[len(YS)//2]
    # Write action to standard output
    print("%s %s" % (X,Y))