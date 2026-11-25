# The backlog and priority

- `-backlogged` / `-shown` and `-priority` set the rest.
  - `-backlogged` moves the workspace into the sidebar's backlog; `-shown`
    moves it back out.
  - `-shown` ends with the workspace drawn in the sidebar, or with an error.
    On an async merge's hidden merge workspace it also reveals it, exactly as
    selecting it does.
  - A reveal is one-way: `-backlogged` on a revealed merge workspace files it
    in the ordinary backlog, where `-shown` brings it back.
