#include <gtest/gtest.h>
#include "message.hpp"
#include "encoder.hpp"

using namespace std;

TEST(MessageTest, Size)
{
    ASSERT_EQ(5, Message(5).size());
    ASSERT_EQ(107, Message(107).size());
}

TEST(MessageTest, SetClearGet)
{
    Message m(64);
    m.set(1);
    m.set(2);
    m.set(3);
    m.set(59);
    ASSERT_FALSE(m.get(0));
    for (int i = 1; i <= 3; ++i)
    {
        ASSERT_TRUE(m.get(i));
    }
    for (int i = 4; i <= 58; ++i)
    {
        ASSERT_FALSE(m.get(i));
    }
    ASSERT_TRUE(m.get(59));
    for (int i = 60; i <= 63; ++i)
    {
        ASSERT_FALSE(m.get(i));
    }

    m.clear(3);
    m.clear(59);
    ASSERT_FALSE(m.get(0));
    for (int i = 1; i <= 2; ++i)
    {
        ASSERT_TRUE(m.get(i));
    }
    for (int i = 3; i <= 63; ++i)
    {
        ASSERT_FALSE(m.get(i));
    }
}

TEST(MessageTest, Cardinality)
{
    Message m(64);
    m.set(0);
    m.set(7);
    m.set(47);
    ASSERT_EQ(3, m.cardinality());
}

TEST(MessageTest, Equals)
{
    Message m1(64);
    m1.set(0);
    m1.set(53);
    Message m2(64);
    m2.set(0);
    m2.set(53);
    ASSERT_TRUE(m1 == m2);
    m2.clear(0);
    ASSERT_FALSE(m1 == m2);
}

TEST(MessageTest, BitwiseAnd)
{
    Message m1(64);
    m1.set(0);
    m1.set(38);
    m1.set(57);
    Message m2(64);
    m2.set(0);
    m2.set(38);
    ASSERT_TRUE(m2 == (m1 & m2));
}

TEST(MessageTest, Reverse)
{
    {
        Message m(64);
        m.set(1);
        m.set(23);
        m.set(58);
        Message expected(64);
        expected.set(5);
        expected.set(40);
        expected.set(62);
        ASSERT_EQ(expected, m.reverse());
    }
    {
        Message m(59);
        m.set(1);
        m.set(23);
        m.set(58);
        Message expected(59);
        expected.set(0);
        expected.set(35);
        expected.set(57);
        ASSERT_EQ(expected, m.reverse());
    }
}

TEST(MessageTest, Submessage)
{
    {
        Message m(64);
        m.set(1);
        m.set(2);
        m.set(35);
        m.set(57);
        Message expected(36);
        expected.set(0);
        expected.set(33);
        ASSERT_EQ(expected, m.submsg(2, 36));
    }
    {
        Message m = Message::parse("b0c152f9 00000000");
        ASSERT_EQ(Message::parse("a97c8000").str(), m.submsg(15, 17).str());
    }
    {
        Message m = Message::parse("b0c152f9");
        ASSERT_EQ(Message::parse("a97c8000").str(), m.submsg(15, 17).str());
    }
}

TEST(MessageTest, String)
{
    Message m(64);
    m.set(5);
    m.set(44);
    ASSERT_EQ("04000000 00080000", m.str());
}

TEST(MessageTest, Parse)
{
    Message m = Message::parse("00000001 00000000");
    for (int i = 0; i <= 30; ++i)
    {
        ASSERT_FALSE(m.get(i));
    }
    ASSERT_TRUE(m.get(31));
    for (int i = 32; i <= 63; ++i)
    {
        ASSERT_FALSE(m.get(i));
    }
}

TEST(EncoderTest, Encode)
{
    Message m1 = Message::parse("00000083 000000e5");
    ASSERT_EQ(Message::parse("000073af 00000000"), encode(m1));
    Message m2 = Message::parse("b0c152f9 ebf2831f");
    ASSERT_EQ(Message::parse("46508fb7 6677e201"), encode(m2));
}

int main(int argc, char **argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
