#include "encoder.hpp"
#include <iostream>

Message encode(const Message& m)
{
   unsigned int size = m.size();
   Message m1 = m.submsg(0, size/2);
   Message m2 = m.submsg(size/2, size/2).reverse();
   
   Message e(size);
   for(int i = size/2 - 1; i > 0; --i)
   {
      {
         Message sub_m1 = m1.submsg(i, size/2 - i);
         Message sub_m2 = m2.submsg(0, size/2 - i);
         Message sub_e = sub_m1 & sub_m2;
         if(sub_e.cardinality() % 2 == 1)
         {
            e.set(i);
         }
	 std::cout << "i     " << i << std::endl;
	 std::cout << "m1    " << sub_m1.str() << std::endl;
	 std::cout << "m2    " << sub_m2.str() << std::endl;
	 std::cout << "sub_e " << sub_e.str() << std::endl;
      }
      {
         Message sub_m1 = m1.submsg(0, size/2 - i);
         Message sub_m2 = m2.submsg(i, size/2 - i);
         Message sub_e = sub_m1 & sub_m2;
         if(sub_e.cardinality() % 2 == 1)
         {
	   //e.set(size/2 + i);
         }
	 //std::cout << sub_m1.str() << std::endl;
	 //std::cout << sub_m2.str() << std::endl;
	 //std::cout << sub_e.str() << std::endl;
      }
   }
   std::cout << m.str() << std::endl;
   std::cout << m1.str() << std::endl;
   std::cout << m2.str() << std::endl;  
   std::cout << e.str() << std::endl;  
   return e;
}
