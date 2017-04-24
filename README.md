# reflex-dhtmlx

Requirements
------------

1. You must include the stylesheet `lib/dhtmlxcalendar.css` in your page
2. You must put everything in `lib/img` in the same directory that you put
   `dhtmlxcalendar.css`.

Updating
--------

If you are to update the javascript blob in libs/dhtmlxcalendar.js, please follow 
these steps to update the exports for google closure compiler:

1. Find all instances of `window.dhx` and change them to `window['dhx']`.
2. Find all instances of `window.dhx4` and change them to `window['dhx4']`.
3. Find all instances of `window.dhtmlxEvent` and change them to `window['dhtmlxEvent']`.
4. Find the definition of the `dhtmlxEvent` function - it should be inside an 

    `if(undefined...){ function dhtmlxEvent...`

   and export dhtmlxEvent by adding `window.dhtmlxEvent=dhtmlxEvent;` immediately after
   the function definition, inside the `if` statement.

Do this for any other symbols that may end up as `undefined` after running your program
through google's closure compiler. 
https://developers.google.com/closure/compiler/docs/api-tutorial3#export

