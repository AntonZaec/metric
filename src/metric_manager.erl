-module(metric_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3]).
-export([start_link/0, get_or_create_mserver/2]).

start_link() ->
	gen_server:start_link({local, metric_manager}, ?MODULE, [], []).

get_or_create_mserver(MetricName, Interval) ->
	case ets:lookup(metric_servers, MetricName) of
		[] -> create_mserver(MetricName, Interval);
		[{MetricName, MetricServer}] -> 
			case is_process_alive(MetricServer) of
				true -> MetricServer;
				false -> create_mserver(MetricName, Interval)
			end
	end.

create_mserver(MetricName, Interval) ->
	MetricServer = gen_server:call(
		metric_manager, {create, MetricName, Interval}),
	MetricServer.

init(_Args) ->
	ets:new(metric_servers, [
		protected, named_table, set, {read_concurrency, true}]),
	{ok, []}.

handle_call({create, MetricName, Interval}, _From, State) ->
	case ets:lookup(metric_servers, MetricName) of
		[] -> 
			MetricServer = metric_server_sup:add_mserver(MetricName, Interval),
			ets:insert(metric_servers, {MetricName, MetricServer}),
			{reply, MetricServer, State};
		[{MetricName, MetricServer}] -> {reply, MetricServer, State}
	end.