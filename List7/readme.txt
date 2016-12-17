Programowanie funkcyjne, lista 7 
Piotr Maślankowski, grupa kzi

Instrukcja kompilacji:
Należy użyć kolejno poleceń:
ocamlc unix.cma scl.mli
ocamlc unix.cma scl.ml
ocamlc unix.cma FileSCL.mli
ocamlc unix.cma FileSCL.ml
ocamlc unix.cma scl.cmo FileSCL.cmo -o client client.ml
ocamlc unix.cma scl.cmo FILESCL.cmo -o server server.ml

Po wykonaniu tych poleceń w bieżącym katalogu znajdą się dwa programy:
client i server.

API clienta:
Dostępne polecenia:
  ping
  send temat tresc_wiadomosci
  get temat

