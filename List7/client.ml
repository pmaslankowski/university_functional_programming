(* Klient poczty działającej na plikach 
 * Piotr Maślankowski
 * Programowanie funkcyjne 2016
 * grupa kzi                    *)

open FileSCL
module FileClient = Scl.Client (FileChannel) (FileSerial)

let main () =
	let parse_query query = 
		let parts = String.split_on_char ' ' query in
		match parts with
			| ["ping"] -> ["ping"]
			| ["send"; subject; content] -> ["zostaw wiadomosc"; subject; content]
			| ["get"; subject] -> ["odczytaj wiadomosc"; subject]
			| _ -> ["error"]
in
begin
	print_string("Wprowadz zapytanie do serwera:\n");
	let query = read_line () in
	let [r] = FileClient.ask ("in.txt", "out.txt") (parse_query query) in
	print_string("Odpowiedz serwera:\n" ^ r)
end

let _ = main ()