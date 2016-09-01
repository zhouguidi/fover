program test
    use fover
    implicit none

    type(version) :: v

    v = version(0, 1, 1, ['alpha', '1    '], ['build', '1849 ']);
    print*,version_tostring(v)
end program test
