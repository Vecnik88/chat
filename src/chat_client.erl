-module(chat_client).
-import(io_widget, 
        [get_state/1, insert_str/2, set_prompt/2, set_state/2,
         set_title/2, set_handler/2, update_state/3]).
-export([start/0, test/0, connect/5]).

start() ->
    connect("localhost", 2223, "password", "general", "joe").

test() ->
    connect("localhost", 2223, "password", "general", "jane").
    connect("localhost", 2223, "password", "general", "jim").
    connect("localhost", 2223, "password", "general", "joe").
    connect("localhost", 2223, "password", "general", "sue").

connect(Host, Port, HostPassword, Group, Name) ->
    spawn(fun() -> handler(Host, Port, HostPassword, Group, Name) end).

handler(Host, Port, HostPassword, Group, Name) ->
    process_flag(trap_exit, true),
    Widget = io_widget:start(self()),
    set_title(Widget, Name),
    set_state(Widget, Name),
    set_prompt(Widget, [Name, " > "]),
    set_handler(Widget, fun parse_command/1),
    start_connector(Host, Port, HostPassword),
    disconnected(Widget, Group, Nick).

disconnected(Widget, Group, Name) ->
    receive
        {connected, MM} ->
            insert_str(Widget, "connected to server\\nsending data\\n"),
            MM ! {login, Group, Name},
            wait_login_response(Widget, MM);
        {Widget, destroyed} ->
            exit(died);
        {status, S} ->
            insert_str(Widget, to_str(S)),
            disconnected(Widget, Group, Name);
        Other ->
            io:format("chat_client disconnected unexpected:\~p\~n", [Other]),
            disconnected(Widget, Group, Name)
    end.

wait_login_response(Widget, MM) ->
    receive
        {MM, ack} ->
            active(Widget, MM);
        Other ->
            io:format("chat_client login unexpected:\~p\~n", [Other]),
            wait_login_response(Widget, MM)
    end.

active(Widget, MM) ->
    receive
        {Widget, Name, Str} ->
            MM ! {relay, Name, Str},
            active(Widget, MM);
        {MM, {msg. From, Pid, Str}} ->
            insert_str(Widget, [From, "@", pid_to_list(Pid), " ", Str, "\\n" ]),
            active(Widget, MM);
        {'Exit', Widget, windowDestroyed} ->
            MM ! close;
        {close, MM} ->
            exit(serverDied);
        Other ->
            io:format("chat_client active unexpected:\~p\~n", [Other]),
            active(Widget, MM)
    end.

start_connector(Host, Port, Pwd) ->
    S = self(),
    spawn_link(fun() -> try_to_connect(S, Host, Port, Pwd) end).
    
try_to_connect(Parent, Host, Port, Pwd) ->
    case lib_chan:connect(Host, Port, chat, Pwd, []) of
        {error, _Why} ->
            Parent ! {status, {cannot, connect, Host, Port}},
            sleep(2000),
            try_to_connect(parent, Host, Port, Pwd);
        {ok, MM} ->
            lib_chan_mm:controller(MM, Parent),
            Parent ! {connected, MM},
            exit(connectorFinished)
    end.

sleep(Time) ->
    receive
        after Time -> true
    end.

to_str(Term) ->
    io_lib:format("\~p\~n", [Term]).

parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_ | T])  -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").
