# Specify the minimum version for CMake

cmake_minimum_required(VERSION 3.8)

set(PROJ_NAME VMTranslator)

project(${PROJ_NAME})

# The following folder will be included
include_directories("${PROJECT_SOURCE_DIR}")

add_executable(${PROJ_NAME} ${PROJECT_SOURCE_DIR}/main.cpp)

set(CMAKE_SUPPRESS_REGENERATION true)
