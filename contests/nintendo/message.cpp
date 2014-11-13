#include <math.h>
#include "message.hpp"
#include "utils.hpp"

Message::Message(unsigned int size) : size_(size), fragments_(ceil(size*1.0/32))
{
}

unsigned int Message::size() const
{
   return size_;
}

Message Message::reverse() const
{
   Message r(size_);
   int index = 0;
   for(auto i = fragments_.rbegin(); i != fragments_.rend(); ++i)
   {
      if(index > 0)
      {
	r.fragments_[index - 1] |= (Utils::reverse_bits(*i) >> (31 - (size_ % 32)));
      }
      r.fragments_[index] |= (Utils::reverse_bits(*i) << (size_ % 32));
      ++index;
   }
   return r;
}

Message Message::submsg(unsigned int pos, unsigned int len) const
{
   Message s(len);
   int index = 0;
   for(int i = pos; i < pos + len; i += 32)
   {
      unsigned int fragment = fragments_[i];
      if(index > 0)
      {
         s.fragments_[index - 1] |= (fragment >> (31 - (pos % 32)));
      }
      s.fragments_[index] |= ((fragment >> (pos % 32)) << (pos % 32));
      ++index;
   }
   return s;
}

bool Message::operator ==(const Message& other) const
{
   return size_ == other.size_ && fragments_ == other.fragments_;
}

Message Message::operator &(const Message& other) const
{
   Message m(std::min(size_, other.size_));
   for(int i = 0; i < std::min(fragments_.size(), other.fragments_.size()); ++i)
   {
      m.fragments_[i] = fragments_[i] & other.fragments_[i];
   }
   return m;
}

unsigned int Message::cardinality() const
{
   unsigned int c = 0;
   for(auto i = fragments_.begin(); i != fragments_.end(); ++i)
   {
      c += Utils::count_bits(*i);
   }
   return c;
}

bool Message::get(unsigned int pos) const
{
   if(pos >= size_)
   {
      return false;
   }
   return (fragments_[pos / 32] & (1 << (31 - (pos % 32))));
}

void Message::set(unsigned int pos)
{
   fragments_[pos / 32] |= (1 << (31 - (pos % 32)));
}

void Message::clear(unsigned int pos)
{
   fragments_[pos / 32] &= ~(1 << (31 - (pos % 32)));
}
