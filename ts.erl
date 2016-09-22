-module(ts).
-export([new/0, in/2, out/2]).

new() ->
  0.

in(TS, Pattern) ->
  0.

out(TS, Tuple) ->
  0.

server(L) ->
  receive
    {Pid, Pattern} when match(Pattern, L) ->
      Pid ! 0;
    {Tuple} -> server(L ++ Tuple)
  end.

% Match method from Erlang?
  match(any,_) -> true;
  match(P,Q) when is_tuple(P), is_tuple(Q)
                  -> match(tuple_to_list(P),tuple_to_list(Q));
  match([P|PS],[L|LS]) -> case match(P,L) of
                                true -> match(PS,LS);
                                false -> false
                           end;
  match(P,P) -> true;
  match(,) -> false.
