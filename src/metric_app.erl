-module(metric_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).
%% Metric API
-export([report/2, average/1]).

start(_StartType, _StartArgs) ->
    metric_sup:start_link().

stop(_State) ->
    ok.

report(MetricName, Value) -> 
	{ok, Interval} = application:get_env(metric, interval_ms),
	MetricServer = metric_manager:get_or_create_mserver(MetricName, Interval),
	metric_server:report(MetricServer, MetricName, Value),
	ok.

average(MetricName) ->
	{ok, Interval} = application:get_env(metric, interval_ms),
	MetricServer = metric_manager:get_or_create_mserver(MetricName, Interval),
	metric_server:average(MetricServer, MetricName).