Observer Web
============
Observer Web is erlang observer web frontend, base code borrowed from observer gui.

## Feature
Currently supported:
* System
* Load Charts
* Memory Allocators
* Processes (preview)

## TODO

- [x] Applications
- [x] Processes
- [x] Process info
- [x] Port info
- [x] Table viewer
- [ ] Trace Overview

## Usage
Build and relese.
```bash
make rel
```
To start the release with a shell.
```bash
docker run -it --rm -p 8080:8080 observerweb console
```
To start the release without a shell.
```bash
docker run -d --rm -p 8080:8080 observerweb foreground
```
Open http://127.0.0.1:8080 in your browser


## License

    The MIT License (MIT)

Checkout http://package.elm-lang.org/packages/terezka/elm-plot/5.1.0/
