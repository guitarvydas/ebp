An example of EBP, event-based programming.

Communicating state machines.

A software component resembles, on the outside, a digital logic IC.  It has a number of input pins and a number of output pins.

To implement this in software, I use a single input queue, a case statement, and a single output queue.

When an event is sent to a component, the event is tagged with the input pin that the event data is destined for, then the {pin, data} event is placed on the component's input queue.

The component cannot ask for input.  It processes events in a FIFO manner.  A scheduler pulls the first event from the component's input queue, then invokes the component to process that event (and only that event).

The component can produce as many output events {out-pin, data} as it wishes.  These outputs are deferred (borrowed from Turing+ deferred monitors concept).  Hence, outputs are non-blocking.  The output events are placed on the component's output queue.  When the component finishes processing a single input event, the scheduler pops all of the output events from the output queue and distributes them to the input queues of the receiving components.

The component's output pins are mapped to nets of receivers.

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
