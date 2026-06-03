cmake_minimum_required(VERSION 3.10)

project(I VERSION 1.0 LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# set(CMAKE_C_STANDARD 99)
# set(CMAKE_C_STANDARD_REQUIRED True)

include_directories(extern/haikal/src/meta_arena/gen)
include_directories(extern/haikal/src/runtime)

add_subdirectory(extern/haikal)

add_executable(${PROJECT_NAME}
    src/main.c
    src/string8.c
    src/string8slice.c
)
add_dependencies(${PROJECT_NAME} haikal)

set(LLVM_ROOT "C:/Program Files/LLVM" CACHE PATH "LLVM install root")
find_path(LIBCLANG_INCLUDE_DIR clang-c/Index.h
    PATHS "${LLVM_ROOT}/include"
    NO_DEFAULT_PATH
)
find_library(LIBCLANG_LIBRARY libclang
    PATHS "${LLVM_ROOT}/lib"
    NO_DEFAULT_PATH
)

if (LIBCLANG_INCLUDE_DIR AND LIBCLANG_LIBRARY)
    add_executable(ibind src/ibind.c)
    target_include_directories(ibind PRIVATE ${LIBCLANG_INCLUDE_DIR})
    target_link_libraries(ibind PRIVATE ${LIBCLANG_LIBRARY})
    if (WIN32)
        target_compile_definitions(ibind PRIVATE _CRT_SECURE_NO_WARNINGS)
    endif()
    if (WIN32)
        add_custom_command(TARGET ibind POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${LLVM_ROOT}/bin/libclang.dll"
                "$<TARGET_FILE_DIR:ibind>/libclang.dll"
        )
    endif()
else()
    message(WARNING "libclang not found; ibind will not be built")
endif()

if (WIN32)
    target_compile_definitions(${PROJECT_NAME} PRIVATE _CRT_SECURE_NO_WARNINGS)
    if (CMAKE_C_COMPILER_ID MATCHES "Clang")
        target_compile_options(${PROJECT_NAME} PRIVATE -Wno-macro-redefined)
    endif()
endif()
# add_subdirectory(extern) # in case of modules...

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
endif()

# add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/resource ${CMAKE_CURRENT_BINARY_DIR}/resource)
