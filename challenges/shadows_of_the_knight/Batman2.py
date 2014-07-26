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

def move(Z2, Z1, ZS, ZMAX, code):
    print(Z1, file=sys.stderr)
    print(Z2, file=sys.stderr)
    if Z1!=Z2:
        if code == "WARMER":
            ZS = [z for z in ZS if abs(z-Z2) < abs(z-Z1)]
        elif code == "COLDER":
            ZS = [z for z in ZS if abs(z-Z1) < abs(z-Z2)]
        elif code == "SAME":
            print(ZS, file=sys.stderr)
            ZS = [z for z in ZS if Z1 <= z and z <= Z2]
            ZS = [ZS[len(ZS)//2]]
            print(ZS, file=sys.stderr)
    
    Z1 = Z2
    if len(ZS)==1:
        Z2 = ZS[0]
    elif Z2 <= ZS[0]:
        Z2 = min(ZMAX-1, ZS[-1] + ZS[0] - Z2)
    elif Z2 >= ZS[-1]:
        Z2 = max(0, ZS[0] - (Z2 - ZS[-1]))
    else:
        Z2 = ZS[0]

    return Z2, Z1, ZS

while 1:
    # Read information from standard input
    code = input()
    print("code=%s" % code, file=sys.stderr)

    # Compute logic here
    if not bomb_x_found:
        X2, X1, XS = move(X2, X1, XS, W, code)
        if len(XS)==1:
            bomb_x_found = True
    else:
        Y2, Y1, YS = move(Y2, Y1, YS, H, code)

    # Write action to standard output
    print("%s %s" % (X2,Y2))
