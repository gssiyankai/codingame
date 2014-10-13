rm -f a.out
g++ -O3 *.cpp -o a.out
if [ -f a.out ]
then
   ./a.out
fi

