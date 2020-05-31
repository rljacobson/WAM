# Let’s write an automated theorem prover!

This is meant to be a series of articles about implementing an automated theorem prover. The theorem prover is realized by implementing a Prolog-like language in Rust using Warren’s Abstract Machine. The `M_n` branches are for different versions of the VM of increasing complexity in `n`.

1. [Introduction](doc/introduction.md)
2. [Supporting infrastructure: parsing and flattening terms](doc/infrastructure.md)
3. [Machine $M_0$](doc/M0.md)
4. Machine $M_1$
5. Machine $M_2$

If you would like to work along with me, I encourage you to use whichever programming language you are most familiar with. I chose to use Rust because I knew I would learn a lot about the Rust language this way.

Issues and pull requests are welcome, whether to report or fix errors *or* to suggest general improvements. There is *always* a better way to teach or do something!

### Text License

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img
alt="Creative Commons License" style="border-width:0"
src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />This work is licensed
under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative
Commons Attribution-ShareAlike 4.0 International License</a>.

### Software License



This software is released under the MIT license:

Copyright 2020 Robert Jacobson

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the  "Software"), to deal in the Software
without restriction, including  without limitation the rights to use, copy, modify, merge,
publish,  distribute, sublicense, and/or sell copies of the Software, and to  permit
persons to whom the Software is furnished to do so, subject to  the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  EXPRESS OR IMPLIED, INCLUDING
BUT NOT LIMITED TO THE WARRANTIES OF  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
