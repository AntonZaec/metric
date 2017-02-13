-module(test_metric_app).
-include_lib("eunit/include/eunit.hrl").
-import(metric_app, [average/1, report/2]).

-define(TIMEOUT, 2000).

average_for_new_metric_test() ->
	test_start(),
	?assert(float_eq(average(<<"new_metric">>), 0)),
	test_stop().
	
average_for_single_report_test() ->
	test_start(),
	report(<<"new_metric">>, 11),
	?assert(float_eq(average(<<"new_metric">>), 11)),
	test_stop().

average_for_two_metrics_test() ->
	test_start(),
	report(<<"first_metric">>, 10),
	report(<<"second_metric">>, -10),
	?assert(float_eq(average(<<"first_metric">>), 10)),
	?assert(float_eq(average(<<"second_metric">>), -10)),
	test_stop().

average_changing() ->
	test_start(),
	MetricName = <<"new_metric">>,
	report(MetricName, 1),
	timer:sleep(?TIMEOUT div 2),
	report(MetricName, 2),
	?assert(float_eq(average(MetricName), 1.5)),
	timer:sleep((?TIMEOUT div 2) + 1),
	?assert(float_eq(average(MetricName), 2)),
	timer:sleep((?TIMEOUT div 2)),
	?assert(float_eq(average(MetricName), 0)),
	test_stop().

average_changing_test_() ->
	{timeout, ?TIMEOUT + 10, [fun average_changing/0]}.

test_start() ->
	application:start(metric),
	application:set_env(metric, interval_ms, ?TIMEOUT).

test_stop() ->
	application:stop(metric).

float_eq(Val1, Val2) ->
	abs(Val1 - Val2) < 1.0e-9.