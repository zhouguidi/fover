program test
  use fover
  implicit none

  type(version) :: v

  v = version(0, 0, 2, ['alpha', '1    '], ['build', '23   '])

  write(*,*)version_tostring(v)
end program test

