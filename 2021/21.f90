subroutine solver21(part,input,ans)
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: int128 = selected_int_kind(32)
  integer(int64) :: ans
  integer        :: part, idx
  type(char_p)   :: input(:)
  integer        :: player1, p1_sc
  integer        :: player2, p2_sc
  integer(int64) :: die_roll
  integer(int128) :: chances(0:21,10,2)
  integer(int128) :: newchances(0:21,10)
  integer(int64) :: won(2), tmp, unwon
  integer        :: score, die, die2, die3, loc, loc_m, score_m 
  integer        :: player, other_player, endval

  idx = index(input(1)%p,":") + 2
  read(input(1)%p(idx:),*) player1
  read(input(2)%p(idx:),*) player2

  if (part == 2) goto 2

  die_roll = 0
  p1_sc = 0
  p2_sc = 0

  do
    player1 = mod(player1 + roll()-1, 10)+1
    p1_sc = p1_sc + player1
    if (p1_sc >= 1000) exit
    player2 = mod(player2 + roll()-1,10)+1
    p2_sc = p2_sc + player2
    if (p2_sc >= 1000) exit
  end do

  ans = min(p1_sc,p2_sc)*die_roll*3
  return

2 continue
  
  unwon = 1
  won = 0
  endval = 21
  chances = 0
  chances(0,player1,1) = 1
  chances(0,player2,2) = 1
  do 
    do player = 1,2
      other_player = mod(player,2)+1
      newchances = 0
      unwon = unwon * 27
      do loc = 1, 10
        do score = 0, endval
          do die = 1,3
            do die2 = 1,3
              do die3 = 1,3
                loc_m = mod(loc + die + die2 + die3 -1,10)+1
                score_m = min(endval,loc_m+score)
                newchances(score_m, loc_m) = newchances(score_m, loc_m) &
                                           + chances(score, loc, player) 
              end do
            end do
          end do
        end do
      end do

      tmp = int(sum(newchances(endval,:)),int64)
      
      won(player) = won(player) + tmp
      unwon = unwon - tmp

      newchances(endval,:) = 0 
      chances(:,:,player) = newchances(:,:)

      if (unwon <= 0) goto 3

      chances(:,:,other_player) = chances(:,:,other_player)  * 27
      chances(:,:,other_player) = chances(:,:,other_player) &
              * unwon /sum( chances(:,:,other_player))

    end do
  end do

3 continue

  ans = maxval(won)
  

contains
  function roll()
    integer roll
    integer, save :: d100 = 1
    roll = d100
    d100 = mod(d100,100) + 1
    roll = roll + d100
    d100 = mod(d100,100) + 1
    roll = roll + d100
    d100 = mod(d100,100) + 1
    die_roll =  die_roll + 1
  end function
end subroutine
