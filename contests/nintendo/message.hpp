#ifndef MESSAGE
#define MESSAGE

#include <string>
#include <vector>

class Message
{
public:
    static Message parse(const std::string &);

    Message(unsigned int size);

    void set(unsigned int pos);

    void clear(unsigned int pos);

    bool get(unsigned int pos) const;

    unsigned int size() const;

    Message reverse() const;

    Message submsg(unsigned int pos, unsigned int len) const;

    bool operator==(const Message &) const;

    Message operator&(const Message &) const;

    unsigned int cardinality() const;

    std::string str() const;

private:
    unsigned int size_;
    std::vector<unsigned int> fragments_;
};

#endif
