(* Serwer poczty działającej na plikach 
 * Piotr Maślankowski
 * Programowanie funkcyjne 2016
 * grupa kzi                    *)

open FileSCL
module FileServer = Scl.Server (FileChannel) (FileSerial) (FileProcessor)

let main () =
	FileServer.run ("out.txt", "in.txt")

let _ = main ()