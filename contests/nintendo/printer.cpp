#include <iostream>
#include <iomanip>
#include "printer.hpp"

using namespace std;

void print(const Message& m)
{
   for(unsigned int i = 0; i < m.size(); i += 32)
   {
      unsigned int fragment = 0;
      for(unsigned int j = 0; j < 32; ++j)
      {
         if(m.get(i + j))
         {
	   fragment |= (1 << (31 - j));
         }
      }
      cout << hex << setfill('0') << setw(8) << fragment << " ";
   }
   cout << endl;
}
