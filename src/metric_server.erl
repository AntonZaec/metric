-module(metric_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/2, report/3, average/2]).

-ifdef(debug).
-define(PRINT_DEBUG(FormatStr, Args), io:format(FormatStr, Args)).
-else.
-define(PRINT_DEBUG(FormatStr, Args), ok).
-endif.
%% Start metric's server
start_link(Name, Interval) ->
	gen_server:start_link(?MODULE, [Name, Interval], []).
%% Report metric value
report(MetricServer, MetricName, Value) ->
	gen_server:cast(MetricServer, {metric_report, MetricName, Value}),
	ok.
%% Get average of metric's values
average(MetricServer, MetricName) ->
	TimeOutMs = 3000,
	try
		case gen_server:call(MetricServer, {metric_average, MetricName}, TimeOutMs) of
			no_data -> 0;
			Average -> Average
		end
	catch
		exit:{timeout, Where} ->
			io:format("** Error. Requesting of average failed - timeout (~p).~n", [Where]),
			0
	end.

init([MetricName, Interval]) ->
	{ok, {
		MetricName, %name
		Interval, %interval
		[], %values
		0.0, %sum 
		0}}. %count.

handle_call({metric_average, MetricName}, _From, 
		{MetricName, _, _, _, _} = State) ->
	?PRINT_DEBUG("State when average: ~p. Pid ~p.~n", [State, self()]),
	NewState = update_state_for_interval(State),
	{_, _, _, Sum, Count} = NewState,
	case Count of
		0 -> {reply, no_data, NewState};
		_ -> {reply, Sum/Count, NewState}
	end.

handle_cast({metric_report, MetricName, Value}, 
		{MetricName, _, _, _, _} = State) ->
	?PRINT_DEBUG("State when report: ~p. Pid ~p.~n", [State, self()]),
	UpdatedState = update_state_for_interval(State),
	{_, _, Values, Sum, Count} = UpdatedState,
	CurTimeMs = erlang:system_time(milli_seconds),
	NewState = update_state(UpdatedState, Count + 1, 
		Sum + Value, [{CurTimeMs, Value}|Values]),
	{noreply, NewState}.

update_state_for_interval({_, Interval, Values, _, _} = State) ->
	CurTimeMs = erlang:system_time(milli_seconds),
	MinTime = CurTimeMs - Interval,
	NewState = remove_values_before(
		MinTime, update_state_value(State, lists:reverse(Values))),
	update_state_value(NewState, lists:reverse(element(3, NewState))).

remove_values_before(MinTime, 
		{_, _, [{ValTime, Val}|Values], Sum, Count} = State) when ValTime =< MinTime ->
	NewState = update_state(State, Count - 1, Sum - Val, Values),
	remove_values_before(MinTime, NewState);
remove_values_before(_MinTime, State) ->
	State.

update_state(State, Count, Sum, Values) ->
	S1 = update_state_count(State, Count),
	S2 = update_state_sum(S1, Sum),
	update_state_value(S2, Values).

update_state_value(State, NewValue) ->
	setelement(3, State, NewValue).

update_state_count(State, NewCount) ->
	setelement(5, State, NewCount).

update_state_sum(State, NewSum) ->
	setelement(4, State, NewSum).