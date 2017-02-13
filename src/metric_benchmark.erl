-module(metric_benchmark).
-export([create_metric_benchmark/0, benchmark/0]).

-define(METRICS_NUMBER, 10000).
-define(CLIENTS_NUMBER, 100).
-define(CLIENTS_REQ_NUMBER, 1000).

create_metric_benchmark() ->
	Indexes = lists:seq(1, ?METRICS_NUMBER),
	Metrics = [list_to_binary(integer_to_list(X))|| X <- Indexes],
	application:start(metric),
	Start = erlang:system_time(milli_seconds),
	lists:foreach(
		fun(MetricName) -> 
			metric_manager:get_or_create_mserver(MetricName, 1) 
		end, Metrics),
	io:format("Benchmark finished by ~p ms.~n", 
		[erlang:system_time(milli_seconds) - Start]),
	application:stop(metric).

benchmark() ->
	rand:seed(exs1024, 
		{181, 
		erlang:system_time(seconds), 
		erlang:system_time(milli_seconds)}),
	Indexes = lists:seq(1, ?METRICS_NUMBER),
	Metrics = [list_to_binary(integer_to_list(X))|| X <- Indexes],
	application:start(metric),
	Minute = 60000,
	lists:foreach(
		fun(MetricName) -> 
			metric_manager:get_or_create_mserver(MetricName, Minute) 
		end, Metrics),
	process_flag(trap_exit, true),
	Start = erlang:system_time(milli_seconds),
	lists:foreach(
		fun(_A) -> 
			spawn_link(fun run_report_client/0) 
		end, lists:seq(1, ?CLIENTS_NUMBER)),
	lists:foreach(
		fun(_A) -> 
			spawn_link(fun run_average_client/0) 
		end, lists:seq(1, ?CLIENTS_NUMBER)),
	wait_clients(?CLIENTS_NUMBER*2),
	io:format("Benchmark finished by ~p ms.~n", [erlang:system_time(milli_seconds) - Start]),
	application:stop(metric).

run_report_client() ->
	SomeMetricValue = 9,
	lists:foreach(
		fun(_A) -> 
			MetricName = get_random_metric(), 
			metric_app:report(MetricName, SomeMetricValue) 
		end, 
		lists:seq(1, ?CLIENTS_REQ_NUMBER)).

run_average_client() ->
	lists:foreach(
		fun(_A) -> 
			MetricName = get_random_metric(), 
			metric_app:average(MetricName) 
		end, 
		lists:seq(1, ?CLIENTS_REQ_NUMBER)).

get_random_metric() ->
	N = random:uniform(?METRICS_NUMBER),
	list_to_binary(integer_to_list(N)).

wait_clients(0) ->
	ok;
wait_clients(N) ->
	receive
		{'EXIT', _Pid, _Reason} -> ok
	end,
	wait_clients(N-1).