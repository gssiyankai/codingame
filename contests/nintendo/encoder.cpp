#include <vector>
#include "encoder.hpp"
#include "printer.hpp"
#include <iostream>
Message encode(const Message& m)
{
   unsigned int size = m.size();
   Message e(size);
   Message first_half = m.submsg(0, size/2);
   print(first_half);
   Message second_half = m.submsg(size/2, size/2).reverse();
   print(second_half);
   for(int i = 1; i < size/2; ++i)
   {
      Message c = (first_half.submsg(0, i) & second_half.submsg(size/2 - i, i));
      std::cout << "\t";print(first_half.submsg(0, i));
      std::cout << "\t";print(second_half.submsg(size/2 - 1 - i, i));
      std::cout << "\t";print(c);
      if(c.cardinality()%2 == 1)
      {
         e.set(i);
      }
   }

   return e;
}
