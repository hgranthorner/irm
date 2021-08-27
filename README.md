# irm

An interactive `rm`.

I often feel terrified of running large glob `rm` commands. This tool will provide you with an interactive experience, so you know you're only deleting what you want.

## Current Status

Early development, non-functional.

## To build

irm is written in clojure, and will be compiled with GraalVM for suitably fast start up times at the terminal. Please use the link in RESOURCES to get yourself set up with Graal (and native-image), then run 

``` sh
./build.sh
```

to generate a new executable.
