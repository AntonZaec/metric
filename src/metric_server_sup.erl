-module(metric_server_sup).
-behaviour(supervisor).
%% Supervisor callbacks
-export([init/1]).
%% Supervisor API
-export([start_link/0, add_mserver/2]).

start_link() ->
    supervisor:start_link({local, metric_server_sup}, ?MODULE, []).

init(_Args) ->
	ChildSpec = #{
		id => 1,
		start => {metric_server, start_link, []},
		restart => transient,
		shutdown => brutal_kill,
		type => worker
	},
    {ok, {{simple_one_for_one, 1, 60}, [ChildSpec]}}.

add_mserver(MetricName, Interval) ->
	{ok, MetricServer} = supervisor:start_child(
		metric_server_sup, [MetricName, Interval]),
	MetricServer.