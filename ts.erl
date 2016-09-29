-module(ts).
-export([new/0, in/2, out/2, matchList/2, findTuple/2]).

new() ->
  spawn_link(fun() -> server([],[]) end).

% client wants to read in a tuple
in(TS, Pattern) ->
  TS ! {in, self(), Pattern},
  receive
    Tuple -> io:fwrite("Process ~p received ~p from Tuplespace ~p~n", [self(), Tuple, TS]),
      Tuple
  end.

% client emits a tuple
out(TS, Tuple) ->
  TS ! {out, Tuple},
  io:fwrite("Process ~p sent ~p to Tuplespace ~p~n", [self(), Tuple, TS]).

server(Tuples, Waitlist) ->
  receive
    {in, Pid, Pattern} ->
        case matchList(Pattern, Tuples) of
        true ->
          ReturnTuple = findTuple(Pattern, Tuples),
          Pid ! ReturnTuple,
          server(lists:delete(ReturnTuple, Tuples), Waitlist);
        false ->
          server(Tuples, Waitlist ++ [{Pid, Pattern}])
        end;
    {out, Tuple} ->
%       io:fwrite("Tuples = ~p~n", [Tuples ++ [Tuple]]),
       case findPattern(Waitlist, Tuple) of
         false -> server(Tuples ++ [Tuple], Waitlist);
         {Pid, ReturnPattern} -> Pid ! Tuple,
           server(Tuples, lists:delete({Pid, ReturnPattern}, Waitlist))
       end
    %TODO: go through waitlist and check if there's a process that can be woken
  end.

% to find a pattern in the waitlist
findPattern([], _) -> false;
findPattern(Waitlist, T) ->
  case match(element(2, hd(Waitlist)), T) of
    true -> hd(Waitlist);
    false -> findPattern(tl(Waitlist), T)
  end.

% to find the matching tuple in L (tuplespace)
findTuple(_, []) -> false;
findTuple(Pattern, L) ->
  case match(Pattern, hd(L)) of
     true -> hd(L);
     false -> findTuple(Pattern, tl(L))
  end.

% to see if any tuple in L (tuplespace) matches the Pattern
matchList(_, []) -> false;
matchList(Pattern, L) ->
  case match(Pattern, hd(L)) of
     true -> true;
     false -> matchList(Pattern, tl(L))
  end.

% Match method (provided)
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.
