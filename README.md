#Why is this slow?

To test:

```
stack build
./time.sh
```



------------------

#Update October 31 2015

The reactive version is now on par with the foldM version in terms of speed.
The solution, as suggested by the author of reactive-banana, was to
break the audio up into chunks for processing. I have done this, breaking
them into unboxed vectors of doubles. I have also increased the number
of oscillators to 3, and added a low-pass filter controlled by an LFO,
to prove that even with added complexity the reactive version can
maintain it's speed.

The foldM version has also been increased to 3 oscillators, but
does not has a low-pass because I didn't feel like spending the
time adding one.
