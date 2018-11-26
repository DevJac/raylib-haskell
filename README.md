# raylib-haskell
Haskell bindings to [raylib](https://www.raylib.com/)

![raylib screenshot](screenshots/cubesmap_screenshot.png?raw=true "screenshot of cubesmap example")

# Status

About 25% of the raylib API is usable from Haskell. 
There are a couple working examples, one of which is shown above in the screenshot.

This code is not mature (my bindings in particular, raylib itself seems good).
This code is not suitable for any serious projets.
I believe some of the memory management tricks I've done are working only by coincidence,
and I have commented about those cases in the code.

Even so, this project might serve as a good example of how to bind to C from Haskell.
If looking for such an example, review the [raylib API](https://www.raylib.com/cheatsheet/cheatsheet.html),
and then look at how I have bound to it. I would not suggest duplicating what I've done with `Model`, `Mesh`, and `Material`, as I am not satisfied with how those bindings have turned out. In particular, I have not figured out a satisfactory way to free those types.
