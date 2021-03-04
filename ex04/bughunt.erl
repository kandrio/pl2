% ATTENTION: compile with: c(bughunt, [export_all]).
% NOTE: Out of the 50 candidate evaluators, only vector_42() is correct!
% Konstantinos Andriopoulos 
% 03116023
-module(bughunt).
-export([test/1, test_all/1]).
-include_lib("proper/include/proper.hrl").

% Type declarations
-type expr() ::  vector()
			  |  {vector_op(),expr(),expr()}
			  |  {scalar_op(),int_expr(),expr()}.
-type int_expr() :: integer()
                | {norm_op(),expr()}.
-type vector_op() :: 'add'|'sub'|'dot'.
-type scalar_op() :: 'mul'|'div'.
-type norm_op() :: 'norm_one'|'norm_inf'.
-type vector() :: [integer(),...].

% A function to test all possible 'vector_XX' evaluators
test_all(51) -> true;
test_all(CurrentID) ->
    test(CurrentID),
    test_all(CurrentID+1).

% A function to test a 'vector_ID' evaluator
test(Id) ->
   Function   = vectors:vector(Id),
   PROPERTIES = [
       {bughunt:prop_eval_general(Function), 500, "Arithmetic error"}, 
       {bughunt:prop_eval_nested_depth(Function), 200, "Very deep, nested expression"}, 
       {bughunt:prop_eval_long_vector(Function), 100, "Very long vector"},
       {bughunt:prop_eval_empty_vector(Function), 5, "Empty vector"}, 
       {bughunt:prop_eval_zero_division(Function), 100, "Attempted to divide by 0 at some point"}], 
   test_all_properties(Function, PROPERTIES).

% Takes a list of 'PROPERTIES' and runs 'proper:quickcheck()' for each one of them
% until one of the tests fails.
test_all_properties(_, []) -> correct;
test_all_properties(Function, [{PROPERTY, NUMTESTS, COMMENT}|PROPERTIES]) ->
    X = proper:quickcheck(PROPERTY, NUMTESTS),
    case X of
        true ->
            test_all_properties(Function, PROPERTIES);
        false ->
                EXPR = hd(proper:counterexample()),
                {EXPR, correct_eval(EXPR, 0), Function(EXPR), COMMENT}
    end.


% 1) Test for correctness with RANDOM EXPRESSIONS as inputs.
prop_eval_general(Function) -> 
	?FORALL(EXPR, expr(), Function(EXPR) =:= correct_eval(EXPR,0)).


% 2) Test for correctness with DEEPLY NESTED EXPRESSIONS as inputs.
prop_eval_nested_depth(Function) -> 
	?FORALL(EXPR, deep_nested_generator(), Function(EXPR) =:= correct_eval(EXPR,0)).

deep_nested_generator() ->
    ?LET(N, range(97, 103), deep_nested_generator(N)).

deep_nested_generator(0) -> vector(5, integer());
deep_nested_generator(N) ->
    union([{union(['add', 'dot', 'sub']), deep_nested_generator(N - 1), deep_nested_generator(0)}]).


% 4) Test for correctness with LONG VECTORS as inputs.
prop_eval_long_vector(Function) -> 
	?FORALL(EXPR, long_vector_generator(), Function(EXPR) =:= correct_eval(EXPR,0)).

long_vector_generator() ->
    ?LET(N, range(97, 103), vector(N, integer())).


% 5) Test for correctness with EMPTY VECTOR as input.
prop_eval_empty_vector(Function) ->
    ?FORALL(EXPR, empty_vector_generator({scalar_op(),int_expr()}), Function(EXPR) =:= correct_eval(EXPR,0)).

empty_vector_generator(T) ->
    ?LET({OP, INTEXPR}, T, {OP, INTEXPR, []}).


% 6) Test for correctness with DIVISION BY 0 as input.
prop_eval_zero_division(Function) ->
    ?FORALL(EXPR, zero_division_generator(expr()), Function(EXPR) =:= correct_eval(EXPR,0)).

zero_division_generator(T) ->
    ?LET(EXPR, T, ['div', 0, EXPR]).


% This is the correct evaluator (for expr()) that was constructed to test all the other 
% cadidate evaluators.
correct_eval(L, _)      when (length(L) == 0 orelse length(L) > 100) -> error;
correct_eval(_, DEPTH)  when (DEPTH > 99)                            -> error;
correct_eval(L, _)      when (length(L) =< 100) -> L;
correct_eval({'add', EXPR1, EXPR2}, DEPTH) -> 
    _EXPR1 = correct_eval(EXPR1, DEPTH+1),
    _EXPR2 = correct_eval(EXPR2, DEPTH+1),
    if
        _EXPR1 =:= error orelse _EXPR2 =:= error  -> error;  
        length(_EXPR1) =/= length(_EXPR2)         -> error;
        true                                      -> vector_add(_EXPR1, _EXPR2)
    end;
correct_eval({'sub', EXPR1, EXPR2}, DEPTH) -> 
    _EXPR1 = correct_eval(EXPR1, DEPTH+1),
    _EXPR2 = correct_eval(EXPR2, DEPTH+1),
    if
        _EXPR1 =:= error orelse _EXPR2 =:= error orelse length(_EXPR1) =/= length(_EXPR2) -> error;
        true                              -> vector_sub(_EXPR1, _EXPR2)
    end;
correct_eval({'dot', EXPR1, EXPR2}, DEPTH) -> 
    _EXPR1 = correct_eval(EXPR1, DEPTH+1),
    _EXPR2 = correct_eval(EXPR2, DEPTH+1),
    if
        _EXPR1 =:= error orelse _EXPR2 =:= error orelse length(_EXPR1) =/= length(_EXPR2) -> error;
        true                              -> vector_dot(_EXPR1, _EXPR2)
    end;
correct_eval({'mul', EXPR1, EXPR2}, DEPTH) -> 
    SCALAR  = correct_eval_int(EXPR1, DEPTH+1),
    VECTOR  = correct_eval(EXPR2, DEPTH+1),
    if
        (SCALAR =:= error) orelse (VECTOR =:= error) -> error;
        true -> scalar_mul(SCALAR, VECTOR)
    end;
correct_eval({'div', EXPR1, EXPR2}, DEPTH) -> 
    SCALAR = correct_eval_int(EXPR1, DEPTH+1),
    VECTOR = correct_eval(EXPR2, DEPTH+1),
    if
        (SCALAR =:= error) orelse (VECTOR =:= error) -> error;
        true -> scalar_div(SCALAR, VECTOR)
    end.

% This is the correct evaluator (for int_expr()) that was constructed to test all the other 
% cadidate evaluators.
correct_eval_int(_, DEPTH) when (DEPTH > 99) -> error;
correct_eval_int(X, _) when is_integer(X) -> X;
correct_eval_int({'norm_one', EXPR}, DEPTH) -> 
    _EXPR = correct_eval(EXPR, DEPTH+1),
    if
        _EXPR =:= error -> error;
        true -> norm_one(_EXPR)     
    end;
correct_eval_int({'norm_inf', EXPR}, DEPTH) -> 
    _EXPR = correct_eval(EXPR, DEPTH+1),
    if
        _EXPR =:= error -> error;
        true -> norm_inf(0, _EXPR) 
    end.


% Helping functions
vector_add([],[]) -> [];
vector_add([X|XS], [Y|YS]) -> [X+Y|vector_add(XS, YS)].

vector_sub([],[]) -> [];
vector_sub([X|XS], [Y|YS]) -> [X-Y|vector_sub(XS, YS)].

vector_dot([],[]) -> [];
vector_dot([X|XS], [Y|YS]) -> [X*Y|vector_dot(XS, YS)].

scalar_mul(_, []) -> [];
scalar_mul(SCALAR, [X|XS]) -> [SCALAR*X|scalar_mul(SCALAR,XS)].

scalar_div(0, _)       -> error;
scalar_div(_, []) -> [];
scalar_div(SCALAR, [X|XS]) -> [X div SCALAR|scalar_div(SCALAR, XS)]. 

norm_one([])     -> 0;
norm_one([X|XS]) -> abs(X) + norm_one(XS).

norm_inf(MAXIM, []) -> MAXIM;
norm_inf(MAXIM, [X|XS]) -> 
    if 
        MAXIM < abs(X) -> norm_inf(abs(X), XS);
        true           -> norm_inf(MAXIM, XS)
    end.
