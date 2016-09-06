program test
    use fover
    implicit none

    type(version) :: v1, v2

    v1 = version(0, 1, 1, ['alpha', '1    '], ['build', '1849 ']);
    print*,version_tostring(v1)

    v2 = v('0.1.1-alpha.1+build.1849')
    print*,version_tostring(v2)

    print*,version_diff(v('v1.0.0'), version('2.0.0'))
    print*,version_diff(v('v2.0.0'), version('2.1.0'))
    print*,version_diff(v('v2.1.0'), version('2.1.1'))
    print*,version_diff(v('v1.0.0-alpha'), version('1.0.0'))
    print*,version_diff(v('v1.0.0-alpha'), version('1.0.0-alpha.1'))
    print*,version_diff(v('v1.0.0-alpha.1'), version('1.0.0-alpha.beta'))
    print*,version_diff(v('1.0.0-alpha.beta'), version('1.0.0-beta'))
    print*,version_diff(v('1.0.0-beta'), version('1.0.0-beta.2'))
    print*,version_diff(v('1.0.0-beta.2'), version('1.0.0-beta.11'))
    print*,version_diff(v('1.0.0-beta.11'), version('1.0.0-rc.1'))
    print*,version_diff(v('1.0.0-rc.1'), version('1.0.0'))

    print*,version('1.0.0') < version('2.0.0')
    print*,version('2.0.0') < version('2.1.0')
    print*,version('2.1.0') < version('2.1.1')
    print*,version('1.0.0-alpha') < version('1.0.0')
    print*,version('1.0.0-alpha') < version('1.0.0-alpha.1')
    print*,version('1.0.0-alpha.1') < version('1.0.0-alpha.beta')
    print*,version('1.0.0-alpha.beta') < version('1.0.0-beta')
    print*,version('1.0.0-beta') < version('1.0.0-beta.2')
    print*,version('1.0.0-beta.2') < version('1.0.0-beta.11')
    print*,version('1.0.0-beta.11') < version('1.0.0-rc.1')
    print*,version('1.0.0-rc.1') < version('1.0.0')

    print*,version('1.0.0') >= version('2.0.0')
    print*,version('2.0.0') >= version('2.1.0')
    print*,version('2.1.0') >= version('2.1.1')
    print*,version('1.0.0-alpha') >= version('1.0.0')
    print*,version('1.0.0-alpha') >= version('1.0.0-alpha.1')
    print*,version('1.0.0-alpha.1') >= version('1.0.0-alpha.beta')
    print*,version('1.0.0-alpha.beta') >= version('1.0.0-beta')
    print*,version('1.0.0-beta') >= version('1.0.0-beta.2')
    print*,version('1.0.0-beta.2') >= version('1.0.0-beta.11')
    print*,version('1.0.0-beta.11') >= version('1.0.0-rc.1')
    print*,version('1.0.0-rc.1') >= version('1.0.0')

    print*,version('1.0.0-rc.1') == version('1.0.0-rc.1+build.1')
    print*,version('1.0.0-rc.1') /= version('1.0.0-rc.1+build.1')
end program test
