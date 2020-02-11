-module(rabbit_prelaunch_sighandler).
-behaviour(gen_event).

-export([setup/0,
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% CAUTION: Signal handling in this module must be kept consistent
%% with the same handling in rabbitmq-server(8).

%% #{signal => default | ignore | stop}.
-define(SIGNALS,
        #{
          %% SIGHUP is often used to reload the configuration or reopen
          %% log files after they were rotated. We don't support any of
          %% those two cases, so stop RabbitMQ gracefully.
          sighup => stop,
          %% SIGTSTP is triggered by Ctrl+Z to pause a program. We can't
          %% do that, so stop RabbitMQ gracefully.
          sigtstp => stop
         }).

-define(SERVER, erl_signal_server).

setup() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        _ ->
            case lists:member(?MODULE, gen_event:which_handlers(?SERVER)) of
                true  -> ok;
                false -> gen_event:add_handler(?SERVER, ?MODULE, [])
            end
    end.

init(_Args) ->
    maps:fold(
      fun
          (Signal, default, ok) -> os:set_signal(Signal, default);
          (Signal, ignore, ok)  -> os:set_signal(Signal, ignore);
          (Signal, _, ok)       -> os:set_signal(Signal, handle)
      end, ok, ?SIGNALS),
    {ok, #{}}.

handle_event(Signal, State) ->
    case ?SIGNALS of
        #{Signal := stop} ->
            error_logger:info_msg(
              "~s received - shutting down~n",
              [string:uppercase(atom_to_list(Signal))]),
            ok = init:stop();
        _ ->
            error_logger:info_msg(
              "~s received - not handled?~n",
              [string:uppercase(atom_to_list(Signal))])
    end,
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
