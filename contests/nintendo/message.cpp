#include <sstream>
#include <iomanip>
#include <iostream>
#include <math.h>
#include "message.hpp"
#include "utils.hpp"

Message Message::parse(const std::string &data)
{
    std::stringstream s;
    s << data;
    std::vector<unsigned int> fragments;
    unsigned int fragment;
    while (s >> std::hex >> fragment)
    {
        fragments.push_back(fragment);
    }
    Message m(fragments.size() * 32);
    for (int i = 0; i < fragments.size(); ++i)
    {
        unsigned int fragment = fragments[i];
        for (int j = 0; j < 32; ++j)
        {
            if (fragment & (1 << (31 - j)))
            {
                m.set(i * 32 + j);
            }
        }
    }
    return m;
}

Message::Message(unsigned int size) : size_(size), fragments_(ceil(size * 1.0 / 32))
{
}

void Message::set(unsigned int pos)
{
    fragments_[pos / 32] |= (1 << (31 - (pos % 32)));
}

void Message::clear(unsigned int pos)
{
    fragments_[pos / 32] &= ~(1 << (31 - (pos % 32)));
}

bool Message::get(unsigned int pos) const
{
    if (pos >= size_)
    {
        return false;
    }
    return (fragments_[pos / 32] & (1 << (31 - (pos % 32))));
}

unsigned int Message::size() const
{
    return size_;
}

Message Message::reverse() const
{
    Message r(size_);

    const unsigned int left_mask_len = size_ % 32;
    const unsigned int right_mask_len = 32 - left_mask_len;
    const unsigned int left_mask = Utils::mask(left_mask_len);
    const unsigned int right_mask = ~left_mask;

    const unsigned int final_pos = size_ - 1;
    unsigned int index = 0;
    while (true)
    {
        if (index < size_)
        {
            const unsigned int fragment = (fragments_[(final_pos - index) / 32] & left_mask);
            r.fragments_[index / 32] |= Utils::reverse_bits(fragment) << right_mask_len;
            index += left_mask_len;
        }
        else
        {
            break;
        }
        if (index < size_)
        {
            const unsigned int fragment = fragments_[(final_pos - index) / 32] & right_mask;
            r.fragments_[index / 32] |= Utils::reverse_bits(fragment) >> left_mask;

            index += right_mask_len;
        }
        else
        {
            break;
        }
    }

    return r;
}

Message Message::submsg(unsigned int pos, unsigned int len) const
{
    Message s(len);

    const unsigned int left_mask_len = pos % 32;
    const unsigned int right_mask_len = 32 - left_mask_len;
    const unsigned int final_mask_len = len % 32;
    const unsigned int left_mask = Utils::mask(left_mask_len);
    const unsigned int right_mask = ~left_mask;
    const unsigned int final_mask = Utils::mask(final_mask_len);

    unsigned int index = 0;
    while (true)
    {
        if (index < len)
        {
            const unsigned int fragment = (fragments_[(pos + index) / 32] & right_mask) << left_mask_len;
            s.fragments_[index / 32] |= fragment;
            index += right_mask_len;
        }
        else
        {
            break;
        }
        if (index < len)
        {
            const unsigned int fragment = fragments_[(pos + index) / 32] & left_mask;
            s.fragments_[index / 32] |= fragment;

            index += left_mask_len;
        }
        else
        {
            break;
        }
    }
    s.fragments_[index / 32] &= final_mask;

    return s;
}

bool Message::operator==(const Message &other) const
{
    return size_ == other.size_ && fragments_ == other.fragments_;
}

Message Message::operator&(const Message &other) const
{
    Message m(std::min(size_, other.size_));
    for (unsigned int i = 0; i < std::min(fragments_.size(), other.fragments_.size()); ++i)
    {
        m.fragments_[i] = fragments_[i] & other.fragments_[i];
    }
    return m;
}

unsigned int Message::cardinality() const
{
    unsigned int c = 0;
    for (auto i = fragments_.begin(); i != fragments_.end(); ++i)
    {
        c += Utils::count_bits(*i);
    }
    return c;
}

std::string Message::str() const
{
    std::stringstream s;
    for (unsigned int i = 0; i < size(); i += 32)
    {
        unsigned int fragment = 0;
        for (unsigned int j = 0; j < 32; ++j)
        {
            if (get(i + j))
            {
                fragment |= (1 << (31 - j));
            }
        }
        if (i > 0)
        {
            s << " ";
        }
        s << std::hex << std::setfill('0') << std::setw(8) << fragment;
    }

    return s.str();
}
