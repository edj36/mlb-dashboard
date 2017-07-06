run: 
	ocamlbuild -pkgs str,unix,yojson,cohttp.lwt game.byte && ./game.byte

clean:
	ocamlbuild -clean

backend:
	ocamlbuild -pkgs str,unix,yojson,cohttp.lwt backend.native && ./backend.native

client:
	cohttp-server-async

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

clean:
	ocamlbuild -clean