cmake_minimum_required(VERSION 2.6)
 
# Locate GTest
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})
 
# Link runTests with what we want to test and the GTest and pthread library
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
file(GLOB SRCS *.cpp *.hpp)
add_executable(runTests ${SRCS})
target_link_libraries(runTests ${GTEST_LIBRARIES} pthread)
