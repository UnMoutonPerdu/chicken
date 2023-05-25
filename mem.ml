(* La mémoire globale. Initialement de taille 1, au hasard, remplie avec la
   valeur 0. *)
let mem = {
  VmBytecode.size = 1 ;
  VmBytecode.next_free = 0 ;
  VmBytecode.data = Array.make 1 (VmBytecode.VMV_int 0)
} ;;


let new_block alloc_size =
  if mem.next_free + alloc_size >= mem.size then (
    (* Besoin de réallouer de la mémoire, au moins la taille demandée pour
       l'allocation. *)
    let new_size = max (mem.size * 2) (mem.next_free + alloc_size) in
    let new_data = Array.make new_size (VmBytecode.VMV_int 0) in
    (* Recopie de l'ancienne mémoire dans la nouvelle. *)
    Array.blit mem.data 0 new_data 0 mem.size ;
    mem.data <- new_data ;
    (* Mise à jour de la taille. *)
    mem.size <- new_size
   );
  let tmp = mem.next_free in
  mem.next_free <- mem.next_free + alloc_size ;
  tmp
;;
