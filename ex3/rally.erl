-module(rally).
-export([rally/3]).
-include_lib("proper/include/proper.hrl").

generate_speeds(A, B, CURR_SPEED) when A > 0, B > 0 ->
    if 
        (CURR_SPEED - B) =< 0 -> lists:seq(CURR_SPEED+A, 10, -10);
        true -> lists:seq(CURR_SPEED+A, CURR_SPEED-B, -10)
    end.

update_sections([{0,0}], _, _) -> [{0,0}]; %WE HAVE CROSSED THE FINISH LINE
update_sections([{POS,LIM}|REST], SPEED, STEPS) -> 
    if
        SPEED > LIM  -> [{-1,-1}]; 
        STEPS < POS  -> [{POS-STEPS,LIM}|REST];
        STEPS =:= POS-> 
            if 
                REST =:= [{0,0}] -> [{0,-1}]; %WE NEED +1 STEP TO CROSS THE FINISH LINE, [{0,1}] IS A DUMMY FLAG
                true -> REST
            end;
        true         -> update_sections(REST,SPEED,(STEPS-POS))
    end.

%RUNS THE MAIN FUNCTION 'find_min_moves' FOR ALL POSSIBLE SPEEDS FOR THE NEXT STEP
helper(_, _, _, _, []) -> -1;
helper(A, B, NUM_MOVES, SECTIONS, [SPEED|SPEEDS]) -> 
    X = find_min_moves(A, B, NUM_MOVES+1, update_sections(SECTIONS, SPEED, SPEED div 10), SPEED),
    if 
        X =:= -1 -> helper(A, B, NUM_MOVES, SECTIONS, SPEEDS);
        true     -> X
    end.

find_min_moves(_, _, _, [{-1,-1}], _) -> -1;     %WE CROSSED THE SPEED LIMIT, RETURN -1
find_min_moves(_, _, NUM_MOVES, [{0,0}], _)  -> NUM_MOVES;   %WE HAVE CROSSED THE FINISH LINE
find_min_moves(_, _, NUM_MOVES, [{0,-1}], _) -> NUM_MOVES+1; %WE NEED +1 STEP TO CROSS THE FINISH LINE
find_min_moves(A, B, NUM_MOVES, SECTIONS, CURR_SPEED) -> 
    SPEEDSS = generate_speeds(A, B, CURR_SPEED),  %FIND LIST OF POSSIBLE SPEEDS FOR THE NEXT STEP
    helper(A, B, NUM_MOVES, SECTIONS, SPEEDSS).
             
rally(A, B, LIST) -> find_min_moves(A,B,0,LIST,0).    
