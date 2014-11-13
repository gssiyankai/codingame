#include <iostream>
#include "decoder.hpp"
#include "encoder.hpp"
#include "printer.hpp"
#include "message.hpp"

using namespace std;

int main()
{
   int size;
   cin >> size;
 
   Message m(size * 2);;
   for (int i = 0; i < size / 16; ++i)
   {
      unsigned int fragment;
      cin >> hex >> fragment;
      for(int j = 0; j < 32; ++j)
      {
	 if(fragment & (1 << (31 - j)))
         {
 	    m.set(i * 32 + j);
         }
      }
   }

   print(m);

   print(m.reverse());
   //print(m.submsg(16, 32));

   //Message e = encode(m);
   //print(e);

   //Message c = decode(m);
   //print(c);

   return 0;
}
