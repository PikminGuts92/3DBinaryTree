(* =============================================== *)
(* ============ Some example commands ============ *)
(* =============================================== *)

(* Generates 20 nodes *)
val bt = GENERATE(Empty, 20);

(* Adds nodes to tree *)
val bt = ADD((4,5,6), bt);
val bt = ADD((3,1,13), bt);
val bt = ADD((16,20,41), bt);

(* Searches for (4,5,6) node *)
(* -- To show it was added *)
SEARCH((4,5,6), bt);

(* Deletes nodes *)
val bt = DEL(("4", "5", "_"), bt);

(* Searches for (4,5,6) node *)
(* -- To show it was deleted *)
SEARCH((4,5,6), bt);

(* Displays all nodes in tree *)
PRINT(bt);