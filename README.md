# Spiral intersect problem

Kumud was talking to me about this problem today.

I decided to solve it in Clojure because
<em><a href="http://landoflisp.com/">Lisp programming rules!</a></em>

## Description

You have a sequence of one or more positive integers.
These correspond to distances in the directions
*up*, *left*, *down*, *right*, repeat.
If you drew a rectangular-spiral with this information,
figure out if any of the lines cross.

The challenge is to do this in *O(n)* time using *O(1)* space
(not counting the input sequence of course).
