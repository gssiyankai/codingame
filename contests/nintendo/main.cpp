#include <iostream>
#include "encoder.hpp"
#include "printer.hpp"

using namespace std;

int main()
{
  int size;
  cin >> size;
 
  unsigned int* a = new unsigned int[size / 16];
 
  for (int i = 0; i < size / 16; i++) {
    cin >> hex >> a[i];
  }

  unsigned int* b = encode(size, a);
  print(size, b);

  return 0;
}

