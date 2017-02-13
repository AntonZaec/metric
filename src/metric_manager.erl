-module(metric_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3]).
-export([start_link/0, get_or_create_mserver/2]).

%% Start metric manager
start_link() ->
	gen_server:start_link({local, metric_manager}, ?MODULE, [], []).
%% Return existed metric's server or create new.
%% Interval used only for new metrics and mean
%% the period of data storing.
get_or_create_mserver(MetricName, Interval) ->
	CreateNew = fun() -> create_mserver(MetricName, Interval) end,
	do_for_metric(MetricName,
		CreateNew,
		fun(MetricServer) -> MetricServer end).

create_mserver(MetricName, Interval) ->
	MetricServer = gen_server:call(
		metric_manager, {create, MetricName, Interval}),
	MetricServer.

init(_Args) ->
	ets:new(metric_servers, [
		protected, named_table, set, {read_concurrency, true}]),
	{ok, []}.

handle_call({create, MetricName, Interval}, _From, State) ->
	CreateNew = fun() ->
			MetricServer = metric_server_sup:add_mserver(MetricName, Interval),
			ets:insert(metric_servers, {MetricName, MetricServer}),
			{reply, MetricServer, State}
		end,
	do_for_metric(MetricName,
		CreateNew, 
		fun(MetricServer)-> {reply, MetricServer, State} end).

do_for_metric(MetricName, FunIfNotExistsOrNotAlive, FunIfAlive) ->
	case ets:lookup(metric_servers, MetricName) of
		[{MetricName, MetricServer}] -> 
			case is_process_alive(MetricServer) of
				true -> FunIfAlive(MetricServer);
				false -> FunIfNotExistsOrNotAlive()
			end;
		[] -> FunIfNotExistsOrNotAlive()
	end.