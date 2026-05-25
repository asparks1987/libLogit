cmake_minimum_required(VERSION 3.20)

project(libLogit
    VERSION 0.1.0
    DESCRIPTION "Cross-language LOGIT logging SDK"
    LANGUAGES C CXX
)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

option(LIBLOGIT_BUILD_TESTS "Build libLogit native tests" ON)
option(LIBLOGIT_BUILD_EXAMPLES "Build libLogit native examples" ON)
option(LIBLOGIT_INSTALL "Install libLogit native targets" ON)

add_library(liblogit_c STATIC languages/c/liblogit.c)
add_library(libLogit::c ALIAS liblogit_c)
target_compile_features(liblogit_c PUBLIC c_std_11)
target_include_directories(liblogit_c
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/languages/c>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
set_target_properties(liblogit_c PROPERTIES
    OUTPUT_NAME logit
    EXPORT_NAME c
)

add_library(liblogit_cpp INTERFACE)
add_library(libLogit::cpp ALIAS liblogit_cpp)
target_compile_features(liblogit_cpp INTERFACE cxx_std_20)
target_include_directories(liblogit_cpp
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
set_target_properties(liblogit_cpp PROPERTIES EXPORT_NAME cpp)

find_package(nlohmann_json CONFIG QUIET)
if(nlohmann_json_FOUND)
    target_link_libraries(liblogit_cpp INTERFACE nlohmann_json::nlohmann_json)
endif()

if(LIBLOGIT_BUILD_TESTS)
    enable_testing()

    add_executable(liblogit_c_tests languages/c/test_logit.c)
    target_link_libraries(liblogit_c_tests PRIVATE libLogit::c)
    add_test(NAME liblogit.c COMMAND liblogit_c_tests)
    set_tests_properties(liblogit.c PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    add_executable(liblogit_cpp_tests languages/cpp/test_logit.cpp)
    target_link_libraries(liblogit_cpp_tests PRIVATE libLogit::cpp)
    add_test(NAME liblogit.cpp COMMAND liblogit_cpp_tests)
    set_tests_properties(liblogit.cpp PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

if(LIBLOGIT_BUILD_EXAMPLES)
    add_executable(liblogit_c_basic examples/c/basic.c)
    target_link_libraries(liblogit_c_basic PRIVATE libLogit::c)

    add_executable(liblogit_cpp_basic examples/cpp/basic.cpp)
    target_link_libraries(liblogit_cpp_basic PRIVATE libLogit::cpp)
endif()

if(LIBLOGIT_INSTALL)
    install(TARGETS liblogit_c liblogit_cpp
        EXPORT libLogitTargets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    )

    install(FILES
        include/liblogit/logit.h
        include/liblogit/logit.hpp
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/liblogit
    )

    install(FILES
        languages/c/liblogit.h
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/liblogit/c
    )

    install(FILES
        libLogit.h
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/liblogit/cpp
    )

    configure_package_config_file(
        cmake/libLogitConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/libLogitConfig.cmake
        INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/libLogit
    )
    write_basic_package_version_file(
        ${CMAKE_CURRENT_BINARY_DIR}/libLogitConfigVersion.cmake
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion
    )

    install(EXPORT libLogitTargets
        NAMESPACE libLogit::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/libLogit
    )
    install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/libLogitConfig.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/libLogitConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/libLogit
    )
endif()
