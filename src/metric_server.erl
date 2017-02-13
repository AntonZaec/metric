-module(metric_server).
-behaviour(gen_server).
-export([init/2, handle_call/3, handle_cast/2]).
-export([start_link/2, report/3, average/2]).

start_link(Name, Interval) ->
	gen_server:start_link(?MODULE, [Name, Interval], []).

report(MetricServer, MetricName, Value) ->
	gen_sever:cast(MetricServer, {metric_report, MetricName, Value}),
	ok.

average(MetricServer, MetricName) ->
	TimeOutMs = 1000,
	try
		case gen_sever:call(MetricServer, {metric_average, MetricName}, TimeOutMs) of
			no_data -> 0;
			Average -> Average
		end
	catch
		error:Error ->
			io:format("Requesting of average failed: ~p~n", [Error]),
			0
	end.

init(MetricName, Interval) ->
	{ok, #{
		name => MetricName, 
		interval => Interval, 
		values => [], 
		sum => 0.0, 
		count => 0}}.

handle_call({metric_average, MetricName}, _From, #{name := MetricName} = State) ->
	NewState = update_state_for_interval(State),
	#{sum := Sum, count := Count} = NewState,
	case Count of
		0 -> {reply, no_data, NewState};
		_ -> {reply, Sum/Count, NewState}
	end.

handle_cast({metric_report, MetricName, Value}, #{name := MetricName} = State) ->
	NewState = update_state_for_interval(State),
	#{values := Values, sum := Sum, count := Count} = NewState,
	CurTimeMs = erlang:system_time(milli_seconds),
	{noreply, NewState#{
		values := [{CurTimeMs, Value}|Values], 
		sum := Sum + Value, 
		count := Count + 1}}.

update_state_for_interval(#{interval := Interval, values := Values} = State) ->
	CurTimeMs = erlang:system_time(milli_seconds),
	MinTime = CurTimeMs - Interval,
	NewState = remove_values_before(
		MinTime, State#{values := lists:reverse(Values)}),
	#{values := NewValues} = NewState,
	NewState#{values := lists:reverse(NewValues)}.

remove_values_before(MinTime, #{
		values := [{ValTime, Val}|Values], 
		sum := Sum, 
		count := Count} = State) when ValTime =< MinTime ->
	remove_values_before(MinTime, State#{
		values := Values, sum := Sum - Val, count := Count - 1});
remove_values_before(MinTime, State) ->
	State.