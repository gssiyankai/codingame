#include <iostream>
#include <iomanip>

using namespace std;

void print(int size, unsigned int* message) {

  for(int i = 0; i < size / 16; i++)
    cout << hex << setfill('0') << setw(8) << message[i] << " ";

  cout << endl;
}

