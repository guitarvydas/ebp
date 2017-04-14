An example of EBP, event-based programming.

Loose description of the algorithm...

```
scheduler:
loop
  component = pop ready-queue
  ;; processing step
  foreach in-event on component.in-queue do
    process in-event
    push out-event(s) onto output queue
  done-foreach
  ;; distribution of outputs
  foreach out-event on component.out-queue do
    net = component.out-pins[out-event.pin]
    lock net
      foreach receiver on net do
        in-pin = map out-event.pin to receiver's input pin
        new-in-event = { receiver.pin, copy out-event.data }
        enqueue net-in-event onto receiver.in-queue
      done-foreach
    unlock net
  done-foreach
end loop
```

(the locking is necessary only if there is more than one receiver)
