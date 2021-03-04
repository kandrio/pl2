-module(rally).
-export([rally/3, find_min_moves/5, cross_line_property/2, find_moves_length/1, find_road_length/1,
    make_valid_section/1, prop_cross_line/0, speed_limit_helper/3, prop_under_speed_limit/0,
    prop_cross_line_special/0]).
-include_lib("proper/include/proper.hrl").


% MAIN CODE 

rally(A, B, SECTIONS) -> length(find_min_moves(A, B, [], SECTIONS, 0)).

% OUR MAIN FUNTION. IT RETURNS A LIST OF ALL THE MOVES/SPEEDS OF THE CAR DURING THE RACE,
% IN CHRONOLOGICAL ORDER.
find_min_moves(_, _, _, [{-1,-1}], _) -> [-1];       % WE CROSSED THE SPEED LIMIT, RETURN [-1]
find_min_moves(_, _, _, [{0,0}], _) -> [10];         % EMPTY SECTION WAS GIVEN AS INPUT (FOR PROPERTY TESTING TO SUCCEED)
find_min_moves(_, _, MOVES, [{0,-1}], _)  -> lists:reverse([10|MOVES]); % WE ARE ON THE FINISH LINE. WE NEED +1 (DUMMY) MOVE
find_min_moves(_, _, MOVES, [{0,-2}], _)  -> lists:reverse(MOVES);      % WE HAVE CROSSED THE FINISH LINE
find_min_moves(A, B, MOVES, SECTIONS, CURR_SPEED) -> 
    SPEEDSS = generate_speeds(A, B, CURR_SPEED),     % LIST OF ALL POSSIBLE SPEEDS (IN VALID RANGE) FOR THE NEXT MOVE
    helper(A, B, MOVES, SECTIONS, SPEEDSS).

% CREATES LIST OF ALL POSSIBLE SPEEDS IN THE GIVEN RANGE
generate_speeds(A, B, CURR_SPEED) when A > 0, B > 0 ->
    if 
        (CURR_SPEED - B) =< 0 -> lists:seq(CURR_SPEED+A, 10, -10);
        true -> lists:seq(CURR_SPEED+A, CURR_SPEED-B, -10)
    end.

% RUNS THE MAIN FUNCTION 'find_min_moves' FOR ALL POSSIBLE SPEEDS
helper(_, _, _, _, []) -> [-1];
helper(A, B, MOVES, SECTIONS, [SPEED|SPEEDS]) -> 
    UPDATED_SECTIONS = update_sections(SECTIONS, SPEED, SPEED div 10),
    X = find_min_moves(A, B, [SPEED|MOVES], UPDATED_SECTIONS, SPEED),
    if 
        X =:= [-1] -> helper(A, B, MOVES, SECTIONS, SPEEDS);
        true     -> X
    end.

% RETURNS THE NEW/UPDATED SECTIONS MAP AFTER A MOVE
update_sections([{0,0}], _, _) -> [{0,-2}];         %WE HAVE CROSSED THE FINISH LINE
update_sections([{POS,LIM}|REST], SPEED, STEPS) -> 
    if
        SPEED > LIM  -> [{-1,-1}];                  %WE HAVE SURPASSED THE SPEED LIMIT, [{1,1}] IS A DUMMY FLAG
        STEPS < POS  -> [{POS-STEPS,LIM}|REST];
        STEPS =:= POS-> 
            if 
                REST =:= [{0,0}] -> [{0,-1}];       %WE NEED +1 STEP TO CROSS THE FINISH LINE, [{0,1}] IS A DUMMY FLAG
                true -> REST
            end;
        true         -> update_sections(REST,SPEED,(STEPS-POS))
    end.


% CODE FOR PROPERTY TESTING

% 1a. CHECK IF THE CAR ALWAYS CROSSES THE FINISH LINE FOR THE CASE OF SIMPLE RANDOM SECTION MAPS

prop_cross_line() -> 
    ?FORALL(SECTIONS, simple_section({pos_integer(), pos_integer()}), cross_line_property(SECTIONS, find_min_moves(240, 240, [], SECTIONS, 0))).

simple_section(T) ->
    ?LET(L,list(T),make_valid_section(L)).

make_valid_section([]) -> [{0,0}];
make_valid_section([{POS,Y}|REST]) -> % SPEED LIMIT MUST BE MULTIPLE OF 10 AND SMALLER THAN 250
    LIMIT = (Y*10) rem 250, 
    if 
        LIMIT =/= 0 -> [{POS,LIMIT}|make_valid_section(REST)];
        true     -> make_valid_section(REST)
    end.

cross_line_property(SECTIONS, MOVES) -> 
    ROAD_LENGTH     = find_road_length(SECTIONS),
    MOVES_LENGTH    = find_moves_length(MOVES),
    ROAD_LENGTH + 1 =< MOVES_LENGTH. 

find_road_length([])        -> 0;
find_road_length([{A,_}|T]) -> A + find_road_length(T).

find_moves_length([])           -> 0;
find_moves_length([MOVE|MOVES]) -> (MOVE div 10) + find_moves_length(MOVES).

% 1.1 CHECK IF THE CAR ALWAYS CROSSES THE FINISH LINE FOR THE CASE OF
% SECTION MAPS WHERE THE CAR FINISHES EXACTLY ON THE FINISH LINE AND 
% ONE ADDITIONAL MOVE IS REQUIRED.

prop_cross_line_special() -> 
    ?FORALL(SECTIONS, special_section(pos_integer()), cross_line_property(SECTIONS, find_min_moves(240, 240, [], SECTIONS, 0))).

special_section(T) ->
    ?LET(L,list(T),make_special_valid_section(L)).

make_special_valid_section([]) -> [{0,0}];
make_special_valid_section([POS|REST]) -> % SPEED LIMIT MUST BE MULTIPLE OF 10 AND SMALLER THAN 250
    LIMIT = (POS*10) rem 250, 
    if 
        LIMIT =/= 0 -> [{POS,LIMIT}|make_special_valid_section(REST)];
        true     -> make_special_valid_section(REST)
    end.

% 1.2 CHECK IF THE CAR'S SPEED IS ALWAYS UNDER THE SPEED LIMIT

prop_under_speed_limit() -> 
    ?FORALL(SECTIONS, simple_section({pos_integer(), pos_integer()}), speed_limit_property(SECTIONS, find_min_moves(240, 240, [], SECTIONS, 0))).

speed_limit_property([{0,0}], _) -> true;
speed_limit_property(SECTIONS, [MOVE|MOVES]) ->
    speed_limit_helper(SECTIONS, [MOVE|MOVES], MOVE div 10).

speed_limit_helper([{0,0}], [_], _) -> true;
speed_limit_helper([{POS,LIMIT}|REST], [MOVE|MOVES], STEPS_LEFT) ->
    if
        MOVE > LIMIT       -> false;
        STEPS_LEFT < POS   -> speed_limit_property([{POS-STEPS_LEFT,LIMIT}|REST], MOVES);
        STEPS_LEFT =:= POS -> 
            if 
                REST =:= [{0,0}] -> true;
                true -> speed_limit_property(REST, MOVES)
            end;
        true         -> speed_limit_helper(REST,[MOVE|MOVES],(STEPS_LEFT-POS))
    end.
