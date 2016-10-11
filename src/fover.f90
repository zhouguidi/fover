module fover
  implicit none

  integer, parameter :: max_token_length = 100

  type version
    integer :: major
    integer :: minor
    integer :: patch
    character(len=max_token_length), dimension(:), allocatable :: pre
    character(len=max_token_length), dimension(:), allocatable :: meta
  contains

    procedure, public :: toString => version_tostring
    final :: version_clean
  end type version

  interface version
    module procedure version_construct
  end interface version

  interface operator(>)
      module procedure version_greater_than
  end interface operator(>)

  interface operator(<)
      module procedure version_less_than
  end interface operator(<)

  interface operator(==)
      module procedure version_equal_to
  end interface operator(==)

  interface operator(/=)
      module procedure version_unequal_to
  end interface operator(/=)

  interface operator(>=)
      module procedure version_greater_equal
  end interface operator(>=)

  interface operator(<=)
      module procedure version_less_equal
  end interface operator(<=)

  interface v
    module procedure version_construct
  end interface v

  interface inc
    module procedure version_inc
  end interface inc

contains
  function version_empty() result(self)
    type(version) :: self

    self%major = 0
    self%minor = 0
    self%patch = 0
    allocate(self%pre(0))
    allocate(self%meta(0))
  end function version_empty

  function version_construct(str, err, msg) result(self)
    !* constructor
    character(len=*), intent(in) :: str
    integer, optional, intent(out) :: err
    character(len=:), allocatable, optional, intent(out) :: msg
    type(version) :: self

    integer :: val
    character(len=:), allocatable :: msg_text

    self = version_empty()

    val = 0
    msg_text = ""

    self = version_fromString(str, val, msg_text)

    if (val /= 0) then
      if (present(err)) err = val
      if (present(msg)) msg = msg_text
      if (.not. present(err) .and. .not. present(msg)) then
        write(*, *)msg_text
        stop
      end if
    end if

    deallocate(msg_text)
  end function version_construct

  subroutine version_clean(self)
    !* finalizer
    type(version) :: self

    if (allocated(self%pre)) deallocate(self%pre)
    if (allocated(self%meta)) deallocate(self%meta)
  end subroutine version_clean

  function version_valid(ver, msg) result(val)
    !* validate
    type(version), intent(in) :: ver
    character(len=:), allocatable, optional, intent(out) :: msg
    integer :: val

    integer :: itok, ichr, asc
    character(len=:), allocatable :: msg_text
    character(len=max_token_length) :: tok
    character(len=1) :: chr

    val = 0
    msg_text = ""

    if (ver%major < 0) then
      val = 1
      msg_text = "major version is negative"
    else
      if (ver%minor < 0) then
        val = 2
        msg_text = "minor version is negative"
      else
        if (ver%patch < 0) then
          val = 3
          msg_text = "patch version is negative"
        else
          if (allocated(ver%pre) .and. size(ver%pre) /= 0) then
            val = pre_meta_valid(ver%pre, 'pre-release version', 3, msg_text)
          else
            if (allocated(ver%meta) .and. size(ver%meta) /= 0) then
              val = pre_meta_valid(ver%meta, 'build metadata', 5, msg_text)
            end if
          end if
        end if
      end if
    end if

    if (present(msg)) msg = msg_text
    deallocate(msg_text)
  end function version_valid

  subroutine version_checkerror(ver)
    !* check for error and stop if true
    type(version), intent(in) :: ver

    integer :: err
    character(len=:), allocatable :: msg

    err = version_valid(ver, msg)

    if (err /= 0) then
      write(*, *)'FORVER(version_checkerror): ', msg
      deallocate(msg)
      stop
    end if

    deallocate(msg)
  end subroutine version_checkerror

  function version_tostring(ver) result(str)
    !* convert to string
    class(version), intent(in) :: ver
    character(len=:), allocatable :: str

    character(len=1000) :: tmpstr
    integer :: itok

    call version_checkerror(ver)

    write(tmpstr, *)ver%major
    str = trim(adjustl(tmpstr))
    write(tmpstr, *)ver%minor
    str = str // "." // trim(adjustl(tmpstr))
    write(tmpstr, *)ver%patch
    str = str // "." // trim(adjustl(tmpstr))
    if (allocated(ver%pre) .and. size(ver%pre) /= 0) then
      str = str // "-"
      do itok = 1, size(ver%pre) - 1
        str = str // trim(adjustl(ver%pre(itok))) // "."
      end do
      str = str // trim(adjustl(ver%pre(size(ver%pre))))
    end if
    if (allocated(ver%meta) .and. size(ver%meta) /= 0) then
      str = str // "+"
      do itok = 1, size(ver%meta) - 1
        str = str // trim(adjustl(ver%meta(itok))) // "."
      end do
      str = str // trim(adjustl(ver%meta(size(ver%meta))))
    end if
  end function version_tostring

  function version_fromString(str, val, msg) result(ver)
    character(len=*), intent(in) :: str
    integer, intent(out) :: val
    character(len=:), allocatable, intent(out) :: msg
    type(version) :: ver

    character(len=*), parameter :: msgpre = "invalid version string: "
    character(len=:), allocatable :: thestr, main, pre, meta
    character(len=max_token_length), dimension(:), allocatable :: toks_main, toks_pre, toks_meta
    character(len=1) :: chr
    integer :: pos_hyphen, pos_plus, last_dot, ichr

    ver = version_empty()

    val = 0
    msg = ""

    pos_hyphen = 0
    pos_plus = 0
    last_dot = 0

    thestr = str

    if (len(thestr) == 0) then
      val = -1
      msg = "empty string"
      deallocate(thestr)
      return
    else
      if (thestr(1 : 1) == 'v' .or. thestr(1 : 1) == 'V') then
        if (len(thestr) == 1) then
          val = -2
          msg = "empty version"
          deallocate(thestr)
          return
        else
          thestr = thestr(2:len_trim(thestr))
        end if
      end if
    end if

    ! look for - and +
    do ichr = 1, len(thestr)
      chr = thestr(ichr : ichr)
      select case(chr)
      case ('-')    ! first hyphen
        if (pos_hyphen ==0) pos_hyphen = ichr
      case ('+')    ! plus; check multiple; only one is allowed
        if (pos_plus == 0) then
          pos_plus = ichr
        else
          val = 1
          msg = "more than one plus sign"
          exit
        end if
      case (' ')    ! space
        val = 2
        msg = "contains space"
        exit
      end select
    end do

    if (val == 0) then
      if (pos_hyphen == 1) then ! hyphen is the first char
        val = 2
        msg = "main version is empty"
      elseif (pos_hyphen == len(thestr)) then ! hyphen is the last char
        val = 3
        msg = "pre-release is empty"
      end if
    end if

    if (val == 0) then
      if (pos_plus == 1) then ! plus is the first char
        val = 4
        msg = "main version is empty"
      elseif (pos_plus == len(thestr)) then ! plus is the last char
        val = 5
        msg = "build metadata is empty"
      end if
    end if

    if (val == 0) then
      if (pos_hyphen == 0) then  ! no hyphen
        if (pos_plus == 0) then  ! no plus
          main = thestr
          pre = ""
          meta = ""
        else     ! has plus; plus is garanteed not to be 1 or last
          main = thestr(1 : pos_plus - 1)
          pre = ""
          meta = thestr(pos_plus + 1 : len_trim(thestr))
        end if
      else   ! has hyphen; hyphen is garanteed not to be 1 or last
        if (pos_plus == 0) then ! no plus
          main = thestr(1 : pos_hyphen - 1)
          pre = thestr(pos_hyphen + 1 : len_trim(thestr))
          meta = ""
        elseif (pos_plus < pos_hyphen) then ! plus is earlier than hyphen
          val = 6
          msg = "build metadata is before pre-release"
        elseif (pos_plus == pos_hyphen + 1) then ! plus is next to hyphen
          val = 7
          msg = "pre-release is empty"
        else  ! has plus; plus is garanteed not to be 1 or last
          main = thestr(1 : pos_hyphen - 1)
          pre = thestr(pos_hyphen + 1 : pos_plus - 1)
          meta = thestr(pos_plus + 1 : len_trim(thestr))
        end if
      end if
    end if

    ! tokenize
    if (val == 0) then
      call tokenize(main, val, msg, toks_main)
      if (val == 0) then
        select case(size(toks_main))
        case (0)
          val = 8
          msg = "major, minor, and patch versions are missing"
        case (1)
          val = 9
          msg = "minor and patch versions are missing"
        case (2)
          val = 10
          msg = "patch version is missing"
        case (3)
          ! correct
        case default
          val = 11
          msg = "main version contains too many tokens"
        end select
      else
        val = val + 11
        msg = "invalid main version: " // msg
      end if
    end if

    if (val == 0) then
      call tokenize(pre, val, msg, toks_pre)
      if (val /= 0) then
        val = val + 14
        msg = "invalid pre-release version: " // msg
      end if
    end if

    if (val == 0) then
      call tokenize(meta, val, msg, toks_meta)
      if (val /= 0) then
        val = val + 17
        msg = "invalid build metadata: " // msg
      end if
    end if

    ! validation
    if (val == 0) then
      val = main_valid(toks_main, msg)
      if (val /= 0) then
        val = val + 20
      end if
    end if

    if (val == 0) val = pre_meta_valid(toks_pre, "pre-release", 22, msg)

    if (val == 0) val = pre_meta_valid(toks_meta, "build metadata", 25, msg)

    if (val == 0) then    ! all good
      read(toks_main(1), *)ver%major
      read(toks_main(2), *)ver%minor
      read(toks_main(3), *)ver%patch
      ver%pre = toks_pre
      ver%meta = toks_meta
    else                  ! remain empty
      msg = msgpre // msg
    end if

    ! clean up
    if (allocated(thestr)) deallocate(thestr)
    if (allocated(main)) deallocate(main)
    if (allocated(pre)) deallocate(pre)
    if (allocated(meta)) deallocate(meta)
    if (allocated(toks_main)) deallocate(toks_main)
    if (allocated(toks_pre)) deallocate(toks_pre)
    if (allocated(toks_meta)) deallocate(toks_meta)
  end function version_fromString

  function version_greater_than(ver1, ver2) result(gt)
      type(version), intent(in) :: ver1, ver2
      logical :: gt

      gt = version_diff(ver1, ver2) > 0
  end function version_greater_than

  function version_less_than(ver1, ver2) result(lt)
      type(version), intent(in) :: ver1, ver2
      logical :: lt

      lt = version_diff(ver1, ver2) < 0
  end function version_less_than

  function version_equal_to(ver1, ver2) result(eq)
      type(version), intent(in) :: ver1, ver2
      logical :: eq

      eq = version_diff(ver1, ver2) == 0
  end function version_equal_to

  function version_unequal_to(ver1, ver2) result(ne)
      type(version), intent(in) :: ver1, ver2
      logical :: ne

      ne = version_diff(ver1, ver2) /= 0
  end function version_unequal_to

  function version_greater_equal(ver1, ver2) result(ge)
      type(version), intent(in) :: ver1, ver2
      logical :: ge

      ge = version_diff(ver1, ver2) >= 0
  end function version_greater_equal

  function version_less_equal(ver1, ver2) result(le)
      type(version), intent(in) :: ver1, ver2
      logical :: le

      le = version_diff(ver1, ver2) <= 0
  end function version_less_equal

  function version_inc_major(ver1) result(ver2)
    type(version), intent(in) :: ver1
    type(version) :: ver2

    ver2 = version_empty()
    ver2%major = ver1%major + 1
  end function version_inc_major

  function version_inc_minor(ver1) result(ver2)
    type(version), intent(in) :: ver1
    type(version) :: ver2

    ver2 = version_empty()
    ver2%major = ver1%major
    ver2%minor = ver1%minor + 1
  end function version_inc_minor

  function version_inc_patch(ver1) result(ver2)
    type(version), intent(in) :: ver1
    type(version) :: ver2

    ver2 = version_empty()
    ver2%major = ver1%major
    ver2%minor = ver1%minor
    ver2%patch = ver1%patch + 1
  end function version_inc_patch

  function version_inc(ver1, part, err, msg) result(ver2)
    type(version), intent(in) :: ver1
    character(len=*), intent(in) :: part
    integer, optional, intent(out) :: err
    character(len=:), allocatable, optional, intent(out) :: msg
    type(version) :: ver2

    select case (trim(adjustl(part)))
    case ('maj', 'major', 'Maj', 'Major')
      ver2 = version_inc_major(ver1)
      if (present(err)) err = 0
      if (present(msg)) msg = ""
    case ('min', 'minor', 'Min', 'Minor')
      ver2 = version_inc_minor(ver1)
      if (present(err)) err = 0
      if (present(msg)) msg = ""
    case ('pat', 'patch', 'Pat', 'Patch')
      ver2 = version_inc_patch(ver1)
      if (present(err)) err = 0
      if (present(msg)) msg = ""
    case default
      if (present(err)) then
        err = 1
        if (present(msg)) then
          msg = "invalid part name"
        end if
        ver2 = version_empty()
      else
        if (present(msg)) then
          msg = "invalid part name"
          ver2 = version_empty()
        else
          write(*,*)"fover(version_inc): invalid part name"
          stop
        end if
      end if
    end select
  end function version_inc
    
  ! private

  function version_diff(ver1, ver2) result(df)
      type(version), intent(in) :: ver1, ver2
      integer :: df

      integer :: itok, ichr, num1, num2
      character(len=:), allocatable :: tok1, tok2

      if (ver1%major == ver2%major) then
          if (ver1%minor == ver2%minor) then
              if (ver1%patch == ver2%patch) then
                  if (allocated(ver1%pre) .and. size(ver1%pre) /= 0) then     !ver1 has pre
                      if (allocated(ver2%pre) .and. size(ver2%pre) /= 0) then !ver2 has pre
                          do itok = 1, min(size(ver1%pre), size(ver2%pre))
                            tok1 = trim(adjustl(ver1%pre(itok)))
                            tok2 = trim(adjustl(ver2%pre(itok)))
                            if (token_numerical(tok1)) then         !current token of ver1 is numerical
                                if (token_numerical(tok2)) then     !current token of ver2 is numerical
                                    read(tok1, *)num1
                                    read(tok2, *)num2
                                    df = num1 - num2
                                    if (df /= 0) exit
                                else                                          !current token of ver2 is not numerical
                                    df = -80
                                    exit
                                end if
                            else                                              !current token of ver1 is not numerical
                                if (token_numerical(tok2)) then     !current token of ver2 is numerical
                                    df = 80
                                    exit
                                else                                          !current token of ver2 is not numerical
                                    do ichr = 1, min(len(tok1), len(tok2))
                                        df = iachar(tok1(ichr : ichr)) - iachar(tok2(ichr : ichr))
                                        if (df /= 0) exit
                                    end do
                                    if (df == 0) then
                                        df = len(tok1) - len(tok2)
                                    end if

                                    if (df /= 0) exit
                                end if
                            end if
                          end do

                          if (df == 0) then
                              df = (size(ver1%pre) - size(ver2%pre)) * 10
                          end if
                      else                                                    !ver2 doesn't have pre
                          df = -90
                      end if
                  else                                                        !ver1 doesn't have pre
                      if (allocated(ver2%pre) .and. size(ver2%pre) /= 0) then !ver2 has pre
                          df = 90
                      else                                                    !ver2 doesn't have pre
                          df = 0
                      end if
                  end if
              else
                  df = (ver1%patch - ver2%patch) * 100
              end if
          else
              df = (ver1%minor - ver2%minor) * 1000
          end if
      else
          df = (ver1%major - ver2%major) * 10000
      end if

      if (allocated(tok1)) deallocate(tok1)
      if (allocated(tok2)) deallocate(tok2)
  end function version_diff

  subroutine tokenize(str, val, msg, toks, deli) 
    character(len=*), intent(in) :: str
    integer, intent(out) :: val
    character(len=:), allocatable, intent(out) :: msg
    character(len=1), optional, intent(in) :: deli
    character(len=max_token_length), dimension(:), allocatable, intent(out) :: toks
    character(len=max_token_length), dimension(:), allocatable :: tmp_toks

    character(len=1) :: del
    integer :: ichr, last_del, i


    del = '.'
    if (present(deli)) del = deli

    val = 0
    msg = ""

    allocate(tmp_toks(20))

    i=1
    if (len_trim(str) > 0) then
      last_del = 0

      do ichr = 1, len(str)
        if (str(ichr : ichr) == del) then
          if (ichr == 1) then
            val = 1
            msg = "leading delimeter"
            return
          elseif (ichr == len(str)) then
            val = 2
            msg = "trailing delimeter"
            return
          else
            if (last_del == 0) then
              tmp_toks(i) = str(1:ichr-1)
              i = i+1
            else
              if (last_del == ichr - 1) then
                val = 3
                msg = "empty token"
                return
              end if
              tmp_toks(i) = str(last_del + 1 : ichr - 1)
              i = i+1
            end if
            last_del = ichr
          end if
        end if
      end do

      if (last_del == 0) then  ! no delimeter found
        allocate(toks(1))
        toks(1) = str
      else                     ! don't forget the last token
        allocate(toks(i))
        do i=1, size(toks)-1
          toks(i) = tmp_toks(i)
        enddo
        toks(i) = str(last_del + 1 : len_trim(str))
      end if
    end if

  end subroutine tokenize

  function main_valid(main, msg) result(val)
    character(len=max_token_length), dimension(:), intent(in) :: main
    character(len=:), allocatable, intent(out) :: msg
    integer :: val

    integer :: itok
    character(len=:), allocatable :: tok
    character(len=*), dimension(3), parameter :: nms = ["major", "minor", "patch"]

    do itok = 1, 3
      tok = trim(adjustl(main(itok)))
      if (token_numerical(tok)) then
        if (.not. token_numerical_non_zero(tok)) then
          val = 1
          msg = nms(itok) // " version contains leading zero"
          return
        end if
      else
        val = 2
        msg = nms(itok) // " version is not numerical"
        return
      end if
    end do

    val = 0
    msg = ""
    deallocate(tok)
  end function main_valid

  function pre_meta_valid(pre_meta, nm, err_base, msg) result(val)
    !* check if the pre-release or metadata is valid (every token is valid)
    character(len=max_token_length), dimension(:), intent(in) :: pre_meta
    character(len=*), intent(in) :: nm
    integer, intent(in) :: err_base
    character(len=:), allocatable, intent(out) :: msg
    integer :: val

    integer :: itok

    val = 0
    msg = ""

    do itok = 1, size(pre_meta)
      val = token_valid(trim(adjustl(pre_meta(itok))), msg)
      if (val /= 0) then
        val = val + err_base
        msg = nm // " contains " // msg
        return
      end if
    end do
  end function pre_meta_valid

  function token_valid(tok, msg) result(val)
    !* check if token is valid:
    !  1. every character is in the valid set
    !  2. numerical token doesn't contain leading zeros
    character(len=*), intent(in) :: tok
    character(len=:), allocatable, intent(out) :: msg
    integer :: val

    integer :: ichr

    if (len(tok) == 0) then
      val = 1         ! empty token
      msg = "empty token"
      return
    end if

    do ichr = 1, len(tok)
      if (.not. char_valid(tok(ichr : ichr))) then
        val = 2       ! invalid character inside
        msg = "invalid character"
        return
      end if
    end do

    if (token_numerical(tok)) then
      if (.not. token_numerical_non_zero(tok)) then
        val =3
        msg = "numerical token contains leading zero"
        return
      end if
    end if

    val = 0
    msg = ""
  end function token_valid

  function token_numerical(tok) result(ok)
    character(len=*), intent(in) :: tok
    logical :: ok

    integer :: ichr

    ok = .true.

    if (len(tok) == 0) then
      ok = .false.
    else
      do ichr = 1, len(tok)
        if (.not. char_numerical(tok(ichr : ichr))) then
          ok = .false.
          exit
        end if
      end do
    end if
  end function token_numerical

  function token_numerical_non_zero(tok) result(ok)
    character(len=*), intent(in) :: tok
    logical :: ok

    if (token_numerical(tok)) then
      if (len(tok) /= 1) then
        if (tok(1 : 1) == '0') then
          ok = .false.
        else
          ok = .true.
        end if
      else
        ok = .true.
      end if
    else
      ok = .false.
    end if
  end function token_numerical_non_zero

  elemental function char_valid(chr) result(ok)
    !* check if character is in the valid set
    character(len=1), intent(in) :: chr
    logical :: ok

    integer :: asc

    asc = iachar(chr)

    ok = (asc == 45) .or. (asc >= 48 .and. asc <= 57) .or. (asc >= 65 .and. asc <= 90) .or. (asc >= 97 .and. asc <= 122)
  end function char_valid

  function char_numerical(chr) result(ok)
    character(len=1), intent(in) :: chr
    logical :: ok

    integer :: asc

    asc = iachar(chr)

    ok = asc >= 48 .and. asc <= 57
  end function char_numerical

end module fover
