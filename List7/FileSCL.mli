(* Moduł File Server-Client Library
 * Interfejs modułu FileSCL
 * Piotr Maślankowski
 * Programowanie funkcyjne 2016
 * grupa kzi                    *)

open Scl

module FileChannel : (channel with type t_init = string * string
                              and type t = string * string 
                              and type t_data = string )

module FileSerial : (serial with type t = string list and type t_serial = string)

module FileProcessor : (processor with type t = string list)