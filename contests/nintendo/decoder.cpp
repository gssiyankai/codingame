#include "decoder.hpp"

Message decode_brute_force(const Message &m)
{
    return m;
}

Message decode(const Message &m)
{
    return decode_brute_force(m);
}
