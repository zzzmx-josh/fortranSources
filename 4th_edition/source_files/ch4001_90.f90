subroutine obact(todo)
  implicit none
! *** start of declarations inserted by spag
  integer act, length, nchar
! *** end of declarations inserted by spag
  integer todo, done, base
  common /eg1/nchar, length, done
  parameter (base=10)

  do while (todo/=0)
    act = mod(todo, base)
    todo = todo/base
    select case (act)
    case (1, 4, 7, 8, 9)
      call badact(act)
      exit
    case (2)
      call copy
      length = length + nchar
    case (3)
      call move
    case (5)
      nchar = -nchar
      call delete
      length = length + nchar
    case (6)
      call print
    case default
      cycle
    end select
    done = done + 1
    call resync
  end do
  return
end subroutine
