-module(owntest).
-export([test/0, slave/2]).
-import(ts,[in/2,out/2,new/0]).

%% Sample run
%% (node@chalmers)4> c(ts).  
%% {ok,ts}
%% (node@chalmers)5> Pid = ts:new().
%% <0.55.0>
%% (node@chalmers)6> ts:out(Pid, {hello,world}).
%% Process <0.32.0> sent {hello,world} to Tuplespace <0.55.0>
%% ok
%% (node@chalmers)7> ts:out(Pid, {hello,all}).
%% Process <0.32.0> sent {hello,all} to Tuplespace <0.55.0>
%% ok
%% (node@chalmers)8> ts:out(Pid, {goodbye,all}).
%% Process <0.32.0> sent {goodbye,all} to Tuplespace <0.55.0>
%% ok
%% (node@chalmers)9> spawn(ts,in,[Pid,{any,any,any}]).
%% <0.60.0>
%% (node@chalmers)10> spawn(ts,in,[Pid,{any,all}]).    
%% Process <0.62.0> received {hello,all} from Tuplespace <0.55.0>
%% <0.62.0>
%% (node@chalmers)11> spawn(ts,in,[Pid,{any,all}]).
%% Process <0.64.0> received {goodbye,all} from Tuplespace <0.55.0>
%% <0.64.0>
%% (node@chalmers)12> ts:out(Pid, {hello,all,again}).
%% Process <0.32.0> sent {hello,all,again} to Tuplespace <0.55.0>
%% Process <0.60.0> received {hello,all,again} from Tuplespace <0.55.0>
%% ok
%% (node@chalmers)13> spawn(ts,in,[Pid,{any,any}]).    
%% Process <0.67.0> received {hello,world} from Tuplespace <0.55.0>
%% <0.67.0>                              

test() ->
    Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    Slave1 = spawn_in_test(TS, {hello, world, any}),
    sleep(Delay),

    out_test(TS, {hello,fabian}),
    sleep(Delay),

    Slave2 = spawn_in_test(TS, {hello, fabian}),
    sleep(Delay),

    replytest(Slave2, {hello,fabian}, {hello,fabian}),
    sleep(Delay),

    out_test(TS, {hello, world, nancy}),
    sleep(Delay),

    replytest(Slave1, {hello, world, any}, {hello, world, nancy}),
    sleep(Delay),

    Slave3 = spawn_in_test(TS, {any,any}),
    sleep(Delay),

    Slave4 = spawn_in_test(TS, {any,any,any}),
    sleep(Delay),

    receive
	{Slave3, Tup} ->
	    io:format("Error. Empty tuplespace, but received: ~w~n",[Tup]);
        {Slave4, Tup} ->
	    io:format("Error. Empty tuplespace, but received: ~w~n",[Tup])
    after
        1000 ->   
	    io:format("Correct. Tuplespace appears to be empty.~n"),
	    exit(Slave3, this_is_it),
	    exit(Slave4, this_is_it),
	    exit(TS, this_is_it),
	    collect_exits([Slave1, Slave2, Slave3, Slave4, TS]),
	    finished
    end.

%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(T) ->
    receive
    after
	T -> true
    end.

out_test(Tuplespace, Tup) ->
    io:format("TEST: out(TS, ~w)~n", [Tup]),
    out(Tuplespace, Tup).

% spawns a slave task to perform an in test. This function 
% returns the slave's Pid. The slave will forward the result of the 
% in operation to the caller.

spawn_in_test(Tuplespace, Pat) -> 
    S = spawn_link(test, slave, [Tuplespace, {self(), Pat}]),
    io:format("TEST: in(TS, ~w) by process ~w~n", [Pat, S]),
    S.

%% Get a tuple matching Item from Tuplespace T and send it to Pid
slave(T, {Pid,Item}) ->
    case in(T, Item) of
	R -> Pid!{self(), R}
    end.

%% Tests whether the reply from a Slave task matches the expected Tuple
replytest(Slave, Pat, Tup) -> 
    io:format("Process ~w~n", [Slave]),
    receive
	{Slave,Tup} ->
	    io:format("     Correct. in operation: ~w returned tuple: ~w~n", [Pat, Tup]);
        {Slave,Bad} ->
	    io:format("     Error. in with pattern: ~w returned tuple: ~w~n", [Pat, Bad])
    after 
        5000 ->   
	    io:format("     Error. No response for in operation with pattern: ~w~n", [Pat])
    end.

collect_exits([]) ->
    done;
collect_exits([Pid | Pids]) ->
    receive
	{'EXIT', Pid, _} ->
	    collect_exits(Pids)
    end.
