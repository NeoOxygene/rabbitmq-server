-module(rabbit_os_signal_handler).

-behaviour(gen_event).

-export([start_link/0, init/1,
         handle_event/2, handle_call/2, handle_info/2,
         terminate/2]).

%% CAUTION: Signal handling in this module must be kept consistent
%% with the same handling in rabbitmq-server(8).

start_link() ->
    rabbit_log:info("Swapping OS signal event handler (erl_signal_server) for our own"),
    %% delete any previous incarnations, otherwise we would be accumulating
    %% handlers
    _ = gen_event:delete_handler(erl_signal_server, ?MODULE, []),
    %% swap the standard OTP signal handler if there is one
    ok = gen_event:swap_sup_handler(
        erl_signal_server,
        %% what to swap
        {erl_signal_handler, []},
        %% new event handler
        {?MODULE, []}),
    gen_event:start_link({local, ?MODULE}).

init(_) ->
    ok = os:set_signal(sigterm, handle),
    ok = os:set_signal(sighup, handle),
    ok = os:set_signal(sigtstp, handle),
    {ok, #{}}.

handle_event(Signal, State)
  when Signal =:= sigterm orelse
       Signal =:= sighup orelse
       Signal =:= sigtstp ->
    error_logger:info_msg(
      "Received a ~s, will shut down gracefully",
      [string:uppercase(atom_to_list(Signal))]),
    rabbit:stop_and_halt(),
    {ok, State};
handle_event(Msg, S) ->
    %% Delegate all unknown events to the default OTP signal handler.
    %% FIXME: We call erl_signal_handler's internal functions, this is
    %% fragile.
    {ok, ESHState} = erl_signal_handler:init(undefined),
    _ = erl_signal_handler:handle_event(Msg, ESHState),
    {ok, S}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

terminate(_Args, _State) ->
    ok.
