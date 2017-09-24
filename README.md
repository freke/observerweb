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

- [ ] Applications
- [x] Processes
- [x] Process info
- [x] Port info
- [ ] Table viewer
- [ ] Trace Overview

## Usage
Build and relese.
```bash
make rel
```
To start the release in the foreground.
```bash
./_build/default/rel/observerweb/bin/observerweb console
```
Open http://127.0.0.1:8080 in your browser


## License

    The MIT License (MIT)
