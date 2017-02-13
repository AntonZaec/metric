# metric

Example of usage:
```
application:start(metric).
metric_app:report(<<"some_metric1">>, 1).
metric_app:report(<<"some_metric1">>, 2).
metric_app:average(<<"some_metric1">>).
```

Benchmarking:
```
metric_benchmark:benchmark().
```
