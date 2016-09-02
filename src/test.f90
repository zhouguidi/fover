program test
    use fover
    implicit none

    type(version) :: v1, v2

    v1 = version(0, 1, 1, ['alpha', '1    '], ['build', '1849 ']);
    print*,version_tostring(v1)

    v2 = version_construct('0.1.1-alpha.1+build.1849')
    print*,version_tostring(v2)
end program test
