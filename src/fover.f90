module fover
  implicit none

  integer, parameter :: max_token_length = 100

  type version
    integer :: major
    integer :: minor
    integer :: patch
    !TODO: implement as allocatable array of allocatable character
    character(len=max_token_length), dimension(:), allocatable :: pre
    character(len=max_token_length), dimension(:), allocatable :: meta
  contains
    final :: version_clean
  end type version

contains
  !TODO: implement constructor

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
  end function version_valid

  subroutine version_checkerror(ver)
    !* check for error and stop if true
    type(version), intent(in) :: ver

    integer :: err
    character(len=:), allocatable :: msg

    err = version_valid(ver, msg)

    if (err /= 0) then
      write(*, *)'FORVER(version_checkerror): ', msg
      stop
    end if
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
    
  ! private
  function pre_meta_valid(pre_meta, nm, err_base, msg) result(val)
    !* check if the pre-release or metadata is valid (every token is valid)
    character(len=max_token_length), dimension(:), intent(in) :: pre_meta
    character(len=*), intent(in) :: nm
    integer, intent(in) :: err_base
    character(len=:), allocatable, intent(out) :: msg
    integer :: val

    integer :: itok

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
    !* check if token is valid (every character is in the valid set)
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

    val = 0
    msg = ""
  end function token_valid

  elemental function char_valid(chr) result(ok)
    !* check if character is in the valid set
    character(len=1), intent(in) :: chr
    logical :: ok

    integer :: asc

    asc = iachar(chr)

    ok = (asc == 45) .or. (asc >= 48 .and. asc <= 57) .or. (asc >= 65 .and. asc <= 90) .or. (asc >= 97 .and. asc <= 122)
  end function char_valid

end module fover
