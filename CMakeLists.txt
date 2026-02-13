cmake_minimum_required(VERSION 3.10)

project(c-init VERSION 1.0 LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# set(CMAKE_C_STANDARD 99)
# set(CMAKE_C_STANDARD_REQUIRED True)

include_directories(extern/haikal/src/meta_arena/gen)
# include_directories(extern/saha/src)
add_executable(${PROJECT_NAME}
    src/fileops.c
    src/Component.c
    src/main.c
    src/string8.c
    src/string8_containers.c
    src/string8slice.c
    src/string8slice_containers.c
)
# add_subdirectory(extern) # in case of modules...

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
endif()

# add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/resource ${CMAKE_CURRENT_BINARY_DIR}/resource)
