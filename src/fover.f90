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
    final :: version_clean
  end type version

  interface version
    module procedure version_construct
  end interface version

contains
  function version_construct(str, err, msg) result(self)
    !* constructor
    character(len=*), intent(in) :: str
    integer, optional, intent(out) :: err
    character(len=:), allocatable, optional, intent(out) :: msg
    type(version) :: self

    integer :: val
    character(len=:), allocatable :: msg_text

    self%major = 0
    self%minor = 0
    self%patch = 0
    allocate(self%pre(0))
    allocate(self%meta(0))

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
    type(version), intent(in) :: ver
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
    character(len=:), allocatable :: main, pre, meta
    character(len=max_token_length), dimension(:), allocatable :: toks_main, toks_pre, toks_meta
    character(len=1) :: chr
    integer :: pos_hyphen, pos_plus, last_dot, ichr

    val = 0
    msg = ""

    pos_hyphen = 0
    pos_plus = 0
    last_dot = 0

    ! look for - and +
    do ichr = 1, len(str)
      chr = str(ichr : ichr)
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
      elseif (pos_hyphen == len(str)) then ! hyphen is the last char
        val = 3
        msg = "pre-release is empty"
      end if
    end if

    if (val == 0) then
      if (pos_plus == 1) then ! plus is the first char
        val = 4
        msg = "main version is empty"
      elseif (pos_plus == len(str)) then ! plus is the last char
        val = 5
        msg = "build metadata is empty"
      end if
    end if

    if (val == 0) then
      if (pos_hyphen == 0) then  ! no hyphen
        if (pos_plus == 0) then  ! no plus
          main = str
          pre = ""
          meta = ""
        else     ! has plus; plus is garanteed not to be 1 or last
          main = str(1 : pos_plus - 1)
          pre = ""
          meta = str(pos_plus + 1 : )
        end if
      else   ! has hyphen; hyphen is garanteed not to be 1 or last
        if (pos_plus == 0) then ! no plus
          main = str(1 : pos_hyphen - 1)
          pre = str(pos_hyphen + 1 : )
          meta = ""
        elseif (pos_plus < pos_hyphen) then ! plus is earlier than hyphen
          val = 6
          msg = "build metadata is before pre-release"
        elseif (pos_plus == pos_hyphen + 1) then ! plus is next to hyphen
          val = 7
          msg = "pre-release is empty"
        else  ! has plus; plus is garanteed not to be 1 or last
          main = str(1 : pos_hyphen - 1)
          pre = str(pos_hyphen + 1 : pos_plus - 1)
          meta = str(pos_plus + 1 : )
        end if
      end if
    end if

    ! tokenize
    if (val == 0) then
      toks_main = tokenize(main, val, msg)
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
      toks_pre = tokenize(pre, val, msg)
      if (val /= 0) then
        val = val + 14
        msg = "invalid pre-release version: " // msg
      end if
    end if

    if (val == 0) then
      toks_meta = tokenize(meta, val, msg)
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
    else                  ! empty
      msg = msgpre // msg
      ver%major = 0
      ver%minor = 0
      ver%patch = 0
      if (allocated(ver%pre)) deallocate(ver%pre)
      allocate(ver%pre(0))
      if (allocated(ver%meta)) deallocate(ver%meta)
      allocate(ver%meta(0))
    end if

    ! clean up
    if (allocated(main)) deallocate(main)
    if (allocated(pre)) deallocate(pre)
    if (allocated(meta)) deallocate(meta)
    if (allocated(toks_main)) deallocate(toks_main)
    if (allocated(toks_pre)) deallocate(toks_pre)
    if (allocated(toks_meta)) deallocate(toks_meta)
  end function version_fromString
    
  ! private
  function tokenize(str, val, msg, deli) result(toks)
    character(len=*), intent(in) :: str
    integer, intent(out) :: val
    character(len=:), allocatable, intent(out) :: msg
    character(len=1), optional, intent(in) :: deli
    character(len=max_token_length), dimension(:), allocatable :: toks

    character(len=1) :: del
    integer :: ichr, last_del

    del = '.'
    if (present(deli)) del = deli

    val = 0
    msg = ""

    allocate(toks(0))

    if (len(str) /= 0) then
      last_del = 0

      do ichr = 1, len(str)
        if (str(ichr : ichr) == del) then
          if (ichr == 1) then
            val = 1
            msg = "leading delimeter"
            deallocate(toks)
            return
          elseif (ichr == len(str)) then
            val = 2
            msg = "trailing delimeter"
            deallocate(toks)
            return
          else
            if (last_del == 0) then
              toks = [toks, str(1 : ichr - 1)]
            else
              if (last_del == ichr - 1) then
                val = 3
                msg = "empty token"
                deallocate(toks)
                return
              end if
              toks = [toks, str(last_del + 1 : ichr - 1)]
            end if
            last_del = ichr
          end if
        end if
      end do
      
      if (last_del == 0) then  ! no delimeter found
        toks = [str]
      else                     ! don't forget the last token
        toks = [toks, str(last_del + 1 : )]
      end if
    end if
  end function tokenize

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
