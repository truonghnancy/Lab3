-module(ts).
-export([new/0, in/2, out/2]).

new() ->
  spawn_link(fun() -> server([],[]) end).

% client wants to read in a tuple
in(TS, Pattern) ->
  TS ! {self(), Pattern},
  receive
    Tuple -> io:fwrite("Process ~p received Tuple ~p from Tuplespace ~p~n", self(), Tuple, TS)
  end.

% client emits a tuple
out(TS, Tuple) ->
  TS ! Tuple,
  io:fwrite("Process ~p sent Tuple ~p to Tuplespace ~p~n", self(), Tuple, TS).

server(Tuples, Waitlist) ->

  receive
    {Pid, Pattern} -> server(Tuples, Waitlist ++ [{Pid, Pattern}]);
    %TODO: check if this process can be immediately woken
    {Tuple} -> server(Tuples ++ [Tuple], Waitlist)
    %TODO: go through waitlist and check if there's a process that can be woken
  end.

matchList(Pattern, L) ->
  case match(Pattern, hd(L)) of
     true -> true;
     false -> matchList(Pattern, tl(L))
  end.

% Match method
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;
match(P,P) -> true;
match(,) -> false.
