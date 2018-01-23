-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, reverse/2]).

-export([start/2]).

start(C, Name) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    C ! ack,
    self() ! {C, {relay, Name, "I'm starting the group"}},
    group_controller([{C, Nick}]).

delete(Pid, [{Pid, Nick} | T], L) -> {Name, reverse(T, L)};
delete(Pid, [H | T], L)           -> delete(Pid, T, [H | L]);
delete(_, [], L)                  -> {"????", L}.

group_controller([]) ->
    exit(allGone);
groupt_controller(L) ->
    receive
        {C, {relay, Name, Str}} ->
            foreach(fun({Pid, _}) -> Pid ! {msg, Name, C, Str} end, L),
            group_controller(L);
        {login, C, Nick} ->
            controller(C, self()),
            C ! ack,
            self() ! {C, {relay, Name, "I'm joining the group"}},
            group_controller([{C, Nick} | L]);
        {close, C} ->
            {Name, L1} = delete(C, L, []),
            self() ! {C, {relay, Name, "'m leaving the group"}},
            group_controller(L1);
        Any ->
            io:format("group controller received Msg=\~p\~n", [Any]),
            group_controller(L)
    end.
