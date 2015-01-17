#include <gtest/gtest.h>
#include "message.hpp"
#include "encoder.hpp"
#include "utils.hpp"

using namespace std;

TEST(UtilsTest, ReverseBits)
{
    ASSERT_EQ(1<<31, Utils::reverse_bits(1));
    ASSERT_EQ(2830359264, Utils::reverse_bits(123456789));
}

TEST(UtilsTest, CountBits)
{
    ASSERT_EQ(3, Utils::count_bits(7));
    ASSERT_EQ(5, Utils::count_bits(91));
}

TEST(UtilsTest, Mask)
{
    ASSERT_EQ(0xF0000000, Utils::mask(4));
}

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
        Message m(32);
        m.set(1);
        Message expected(32);
        expected.set(30);
        ASSERT_EQ(expected, m.reverse());
    }
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
    ASSERT_EQ(Message::parse("000073af 00000000"), encode(Message::parse("00000001 000073af")));
    ASSERT_EQ(Message::parse("000073af 00000000"), encode(Message::parse("00000083 000000e5")));
    ASSERT_EQ(Message::parse("738377c1 00000000"), encode(Message::parse("0000cd55 0000b0c5")));
    ASSERT_EQ(Message::parse("46508fb7 6677e201"), encode(Message::parse("b0c152f9 ebf2831f")));
    ASSERT_EQ(Message::parse("f3268b49 661859eb 0b324559 65ee6bda"),
       encode(Message::parse("0cf5c2bf 9aba68ef c18fb79b de70eef7")));
    ASSERT_EQ(Message::parse("a91db473 fcea8db4 f3bb434a 8dba2f16 51abc87e 92c44759 5c1a16d3 6111c6f4"),
       encode(Message::parse("c58f4047 d73fe36a 24be2846 e2ebe432 a30d28bd bda19675 3f95d074 b6f69434")));
    ASSERT_EQ(Message::parse("4af6fc33 39029380 465c5267 c72f6a8b 0906e6d0 ca60550f 14a5e47c 42ad10fb 4a3bb446 bb74360a 5ea02b9c 23c68553 3fade253 e270ba24 39e141ad 6c38c43d"),
       encode(Message::parse("320a18d5 b61b13f6 1aaaa61c 0afe2a41 1a4ff107 84cc2efc 956ff31d fa595299 33749a7f 6cc9659d dc503569 ef4d0ef5 73b746c5 b8fb36d3 7616e9d6 b21251c4")));
}

int main(int argc, char **argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
