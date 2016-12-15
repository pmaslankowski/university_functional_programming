(* Moduł File Server - Client Library 
 * Implementacja modułów do obsługi plików
 * Piotr Maślankowski
 * Programowanie funkcyjne 2016
 * grupa kzi                    *)


open Scl

module FileChannel : (channel with type t_init = string * string
                              and type t = string * string 
                              and type t_data = string ) = 
struct
	type t_init = string * string
	type t = string * string
	type t_data = string

	let init id = id

	let close id = ()

	let rec receive (path_in, path_out) = 
		let load_file f = 
			let ic = open_in f in 
			let n = in_channel_length ic in
			let s = String.create n in 
			begin
				really_input ic s 0 n;
				close_in ic;
				s;
			end
		in 
		try 
			load_file path_in
		with sys_error -> (Unix.sleep(1); receive (path_in, path_out))

	let rec send msg (path_in, path_out) =
		let save_file f content = 
			let oc = open_out f in 
			begin
				output_string oc content;
				close_out oc; 
			end
		in 
		try 
			save_file path_out msg
		with sys_error -> (Unix.sleep(1); send msg (path_in, path_out) )
end


module FileSerial : (serial with type t = string list and type t_serial = string) = 
struct
	type t = string list
	type t_serial = string

	let serial str = List.fold_right (fun x acc -> if acc = "" then x else x ^ "#" ^ acc) str ""
	let deserial str = String.split_on_char '#' str
end


module FileProcessor : (processor with type t = string list) = struct
	type t = string list 

	let messages_map : (string * string) list ref = ref []
	let get_message title = 
		let rec find l = 
			match l with 
				| [] -> []
				| (subject, message)::messages -> if subject = title then [message]
																     else find messages 
		in
		find !messages_map

	let process commands = 
		match commands with
			| ["ping"] -> ["pong"]
			| ["zostaw wiadomosc"; subject; content] -> (messages_map := (subject, content)::(!messages_map); 
				                                         ["Wiadomosc wyslana"])
			| ["odczytaj wiadomosc"; subject] -> (print_string ("Odczytuje wiadomosc" ^ subject ^ "\n"); 
		                                          get_message subject)
			| _ -> ["Nieznane polecenie"]
end