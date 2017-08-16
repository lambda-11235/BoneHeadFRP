
# BoneHeadFRP

**Do not use this library!!!** This library was written to be a helpful learning
guide in FRP. It is too inefficient in memory an time usage to be used for any
practical program.

The `BoneHeadFRP.Pure` module assumes that behaviors are literally functions
from time to some value, and that event streams are time, value pairs.
`BoneHeadFRP.Stateful` extends this notion with stateful event streams that
receive new events from outside IO. There are two things that make this
implementation inefficient.

1. All events in an event stream are remembered for the entire life of the
   program, which uses a lot of memory.

2. In the `Stateful` module getting a value from a behavior or event stream
   causes them and all there dependents to recalculate their values.

Also, there are some caveats about using the `Stateful` module. First, using the
`at` function with future time values can give bogus results. Second, all
stateful event streams in the program must be created with the same timer.

While this is probably the worst FRP library ever, I believe it succeeds in
showing how FRP can be easily and directly implemented. Hopefully this will
dispel some of the "black magic" behind FRP. Either that or I'll get a message
from Conal Elliott telling me how I missed the point and confused everyone more.
