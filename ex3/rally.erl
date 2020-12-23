-module(rally).
-export([rally/3]).
-include_lib("proper/include/proper.hrl").

generate_speeds(A, B, CURR_SPEED) when A > 0, B > 0 ->
    if 
        (CURR_SPEED - B) =< 0 -> lists:seq(CURR_SPEED+A, 10, -10);
        true -> lists:seq(CURR_SPEED+A, CURR_SPEED-B, -10)
    end.

update_sections([{0,0}], _, _) -> [{0,0}];         %WE HAVE CROSSED THE FINISH LINE
update_sections([{POS,LIM}|REST], SPEED, STEPS) -> 
    if
        SPEED > LIM  -> [{-1,-1}];                 %WE HAVE SURPASSED THE SPEED LIMIT, [{1,1}] IS A DUMMY FLAG
        STEPS < POS  -> [{POS-STEPS,LIM}|REST];
        STEPS =:= POS-> 
            if 
                REST =:= [{0,0}] -> [{0,-1}];      %WE NEED +1 STEP TO CROSS THE FINISH LINE, [{0,1}] IS A DUMMY FLAG
                true -> REST
            end;
        true         -> update_sections(REST,SPEED,(STEPS-POS))
    end.

%RUNS THE MAIN FUNCTION 'find_min_moves' FOR ALL POSSIBLE SPEEDS FOR THE NEXT MOVE
helper(_, _, _, _, []) -> [-1];
helper(A, B, MOVES, SECTIONS, [SPEED|SPEEDS]) -> 
    X = find_min_moves(A, B, [SPEED|MOVES], update_sections(SECTIONS, SPEED, SPEED div 10), SPEED),
    if 
        X =:= [-1] -> helper(A, B, MOVES, SECTIONS, SPEEDS);
        true     -> X
    end.

find_min_moves(_, _, _, [{-1,-1}], _) -> [-1];       %WE CROSSED THE SPEED LIMIT, RETURN [-1]
find_min_moves(_, _, MOVES, [{0,0}], _)  -> lists:reverse(MOVES);     %WE HAVE CROSSED THE FINISH LINE
find_min_moves(_, _, MOVES, [{0,-1}], _) -> lists:reverse([0|MOVES]); %WE NEED +1 (DUMMY) STEP TO CROSS THE FINISH LINE
find_min_moves(A, B, MOVES, SECTIONS, CURR_SPEED) -> 
    SPEEDSS = generate_speeds(A, B, CURR_SPEED),     %FIND LIST OF POSSIBLE SPEEDS FOR THE NEXT MOVE
    helper(A, B, MOVES, SECTIONS, SPEEDSS).
             
rally(A, B, LIST) -> length(find_min_moves(A,B,[],LIST,0)).