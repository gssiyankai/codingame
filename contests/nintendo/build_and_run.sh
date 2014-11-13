rm -f a.out
g++ -std=c++11 -O3 *.cpp -o a.out
if [ -f a.out ]
then
   ./a.out
fi
