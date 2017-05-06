<h2> Natural and Artificial Inteligence Laboratory Tool kit </h2> 


<h4> Dependencies </h4>
    <p> * Steel bank Common Lisp (sbcl)
    <p> * Quick-lisp
    <p> * Ltk (will be downloaded when loaded if you have quick-lisp installed)
     


<h5> Stick diagram generator </h5>
<h6> How to use </h6>

```lisp
 sbcl --load sticks.lisp
 ;;For create an executable
 (nailtk::compile-ntk)
 ;;For running inside sbcl
 (nailtk::main)

```

