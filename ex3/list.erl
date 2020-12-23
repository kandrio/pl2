-module(list).
-export([last/1]).
-spec last([T,...]) -> T.
last([Element]) -> Element;
last([_|Rest]) -> last(Rest).