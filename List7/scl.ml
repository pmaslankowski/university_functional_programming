(* Moduł Server - Client Library 
 * Piotr Maślankowski
 * Programowanie funkcyjne 2016
 * grupa kzi                    *)

module type channel = sig
	type t
 	type t_init
 	type t_data

	val init : t_init -> t
	val close : t -> unit
	val receive : t -> t_data
	val send : t_data -> t -> unit
end


module type serial = sig
	type t
	type t_serial

	val deserial : t_serial -> t
	val serial : t -> t_serial		
end 


module type processor = sig 
	type t

	val process : t -> t	
end


module Server (C : channel) (S : serial with type t_serial = C.t_data) (P : processor with type t = S.t) = 
	struct 
		let rec run (i : C.t_init) : unit = 
			let id = C.init i in 
				begin
					C.send (S.serial (P.process (S.deserial (C.receive id)))) id;
					C.close id;
					Unix.sleepf(1.0);
					run i;  
				end 
end


module Client (C : channel) (S : serial with type t_serial = C.t_data) = 
struct
		let ask (i : C.t_init) (q : S.t) : S.t = 
			let id = C.init i in 
				begin
					C.send (S.serial q) id;
					Unix.sleepf(1.5);
					let r = C.receive id in
					C.close id;
					S.deserial r; 
				end 
end 
