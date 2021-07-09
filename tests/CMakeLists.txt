add_executable(lp95 DemoLapack95.F90 assert.f90)
target_link_libraries(lp95 PRIVATE lapack95)
add_test(NAME basic COMMAND $<TARGET_FILE:lp95>)
