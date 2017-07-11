# run: 
# 	ocamlbuild -pkgs str,unix,yojson,cohttp.lwt game.byte && ./game.byte

clean:
	ocamlbuild -clean

# starts the server that can listen to requests on localhost
backend:
	ocamlbuild -pkgs str,unix,yojson,cohttp.lwt backend.native && ./backend.native

# this makes a local static fileserver (equal to python -m http.server)
# localhost:
# 	cohttp-server-async

repl:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,unix,yojson,cohttp.lwt repl.native && ./repl.native
