<h2> Natural and Artificial Inteligence Laboratory Tool kit </h2> 


<h4> Dependencies </h4>
    <p> * Steel bank Common Lisp (sbcl)
    <p> * Quick-lisp
    <p> * Ltk (will be downloaded when loaded if you have quick-lisp installed)
     


<h5> Stick diagram generator </h5>
<h5> How to use </h5>

```lisp
 sbcl --load sticks.lisp
 ;;For create an executable
 (nailtk::compile-ntk)
 ;;For running inside sbcl
 (nailtk::main)

```


<h5> How to set window size </h5>

```lisp
 sbcl --load sticks.lisp
 ;;(nailtk::config xsize ysize)
 (nailtk::config 300 400) ;;This need to be done only once, if you dont he will load with default values.
 (nailtk::main)
```


<h5> How to save your files in NailTK </h5>

 <p> When running nailtk you'll have  two formats to save  your file :
 <p> *Nailtk file (.ntk)
 <p> *Postcript image


When saving as ntk file, you can use load function later to keep working from where you stop.
When saving in postscript, you will have a image.

To save, just click in the save button on the top of the screen, it will open a new window.
Then you can enter the file name, we will do the extension for you.

ex: input -> dflt
    output-file -> dflt.ntk

<h5> Loading ntk files </h5>

Just click on the load button, a new window will show up.
Enter the file name, we will try to open it.
If the file could not be found, a message will pop up in your terminal.


    		

