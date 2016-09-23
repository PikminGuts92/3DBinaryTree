(* ============             LEGAL STUFF - NOT REALLY                 ============ *)
(* ====                                                                      ==== *)
(* ==== CS 355: Project 1 - SML 3D-Binary Tree                               ==== *)
(* ====                                                                      ==== *)
(* ==== Written By: Francisco Martinez (and totally not last minute either)  ==== *)
(* ==== Last Updated: Jun 19, 2016 C.E. at 4:05 AM                           ==== *)
(* ====                                                                      ==== *)
(* ============================================================================== *)

(* ************            GENERAL FUNCTION OVERVIEW                 ************ *)
(*                                                                                *)
(*   GENERATE                                                                     *)
(*   ===========                                                                  *)
(* - Description:                                                                 *)
(* -	Generates n number of nodes for 3D-binary tree. Values are 1-50.          *)
(* - Usage:                                                                       *)
(* -	node = GENERATE(node, n)                                                  *)
(*                                                                                *)
(*   ADD                                                                          *)
(*   ===========                                                                  *)
(* - Description:                                                                 *)
(* -	Adds 3-element tuple of integers to 3D-binary tree.                       *)
(* - Usage:                                                                       *)
(* -	node = ADD((x,y,z), node)                                                 *)
(*                                                                                *)
(*   DEL                                                                          *)
(*   ===========                                                                  *)
(* - Description:                                                                 *)
(* -	Deletes 3-element tuple of strings (interpreted as integers)              *)
(* -		from 3D-binary tree.                                                  *)
(* - Usage:                                                                       *)
(* -	node = DEL(("x","y","_"), node) << Removes all sub nodes in y             *)
(* -	node = DEL(("x","_","_"), node) << Removes all sub nodes in x             *)
(* -	node = DEL(("_","_","_"), node) << Removes all all nodes                  *)
(*                                                                                *)
(*   SEARCH                                                                       *)
(*   ===========                                                                  *)
(* - Description:                                                                 *)
(* -	Searches 3D-binary tree from 3-element tuple of integers input.           *)
(* - Usage:                                                                       *)
(* -	SEARCH((x,y,z), node)                                                     *)
(*                                                                                *)
(*   PRINT                                                                        *)
(*   ===========                                                                  *)
(* - Description:                                                                 *)
(* -	Displays all elements of 3D-binary tree in order.                         *)
(* - Usage:                                                                       *)
(* -	PRINT(node)                                                               *)
(*                                                                                *)
(* ****************************************************************************** *)

(*Random number generation code*)
val seed =
	let
		val m=Date.minute(Date.fromTimeLocal(Time.now()))
		val s=Date.second(Date.fromTimeLocal(Time.now()))
	in
		Random.rand(m,s)
	end;

fun getRandInt(n)=Random.randRange(1,n) seed;

(* Binary tree data type *)
datatype btree = Empty | Node of int * btree * btree * btree

(* Gets index of node *)
fun GetNodeIndex(Empty) = 0 : int |
	GetNodeIndex(Node(idx, left, right, center)) = idx : int;

(* Prints binary tree path *)
fun printPath(path : int list) =
	(
		if path = [] then print(") ")
		else if length(path) = 1 then
		(
			print(Int.toString(hd(path)));
			printPath(tl(path))
		)
		else
		(
			print(Int.toString(hd(path)) ^ ", ");
			printPath(tl(path))
		)
	);

(* Prints full binary tree path *)
fun printFullPath(path : int list) =
	(
		print("(");
		printPath(path)
	);

(* Prints node values *)
fun printInorder (Empty, path : int list) = () |
	printInorder (Node(idx, left, right, center), path : int list) =
	(
		printInorder(left, path);
		
		if center = Empty then
			printFullPath(path @ [idx])
		else
			printInorder(center, path @ [idx]);
		
		printInorder(right, path)
		
	);

(* For creating nodes *)
fun AddNode (map : int * int * int, Empty, depth : int, path : int list, show : bool) = 
	(
		if depth = 1 then
		(
			let
				val pp = path @ [#1map]
			in
				if show then
				(
					print("\n");
					printFullPath(pp);
					print(" created")
				)
				else ()
			end;
			
			Node(#1map, Empty, Empty, AddNode(map, Empty, depth + 1, path @ [#1map], show))
		)
			
		else if depth = 2 then
		(
			let
				val pp = path @ [#2map]
			in
				if show then
				(
					print("\n");
					printFullPath(pp);
					print(" created")
				)
				else ()
			end;
			
			Node(#2map, Empty, Empty, AddNode(map, Empty, depth + 1, path @ [#2map], show))
		)
			
		else
		(
			let
				val pp = path @ [#3map]
			in
				if show then
				(
					print("\n");
					printFullPath(pp);
					print(" created\n")
				)
				else ()
			end;
			
			Node(#3map, Empty, Empty, Empty)
		)
	) |
	AddNode (map : int * int * int, Node(idx, left, right, center), depth : int, path : int list, show : bool) =
	(
		let
			val pp = path @ [idx]
		in
			if show then
			(
				print("\n");
				printFullPath(pp)
			)
			else ()
		end;
		
		if depth = 1 then
			if #1map = idx then Node(idx, left, right, AddNode(map, center, depth + 1, path @ [#1map], show))
			
			else if #1map < idx then Node(idx, AddNode(map, left, depth, path, show), right, center)
			
			else Node(idx, left, AddNode(map, right, depth, path, show), center)
			
		else if depth = 2 then
			if #2map = idx then Node(idx, left, right, AddNode(map, center, depth + 1, path @ [#2map], show))
			
			else if #2map < idx then Node(idx, AddNode(map, left, depth, path, show), right, center)
			
			else Node(idx, left, AddNode(map, right, depth, path, show), center)
		
		else
			if #3map = idx then Node(idx, left, right, center)
			
			else if #3map < idx then Node(idx, AddNode(map, left, depth, path, show), right, Empty)
			
			else Node(idx, left, AddNode(map, right, depth, path, show), Empty)
	);

(* For searching nodes *)	
fun SearchNode(map : int * int * int, Empty, depth : int, path : int list) =
	(
		print("\n(");
		print(Int.toString(#1map) ^ ", ");
		print(Int.toString(#2map) ^ ", ");
		print(Int.toString(#3map) ^ ")");
		print(" NOT FOUND\n")
	) |
	SearchNode(map : int * int * int, Node(idx, left, right, center), depth : int, path : int list) =
	(
		print("\n");
		printFullPath(path @ [idx]);
		
		if depth = 1 then
			if #1map = idx then SearchNode(map, center, depth + 1, path @ [idx])
			
			else if #1map < idx then SearchNode(map, left, depth, path)
			
			else SearchNode(map, right, depth, path)
			
		else if depth = 2 then
			if #2map = idx then SearchNode(map, center, depth + 1, path @ [idx])
			
			else if #2map < idx then SearchNode(map, left, depth, path)
			
			else SearchNode(map, right, depth, path)
		
		else
			if #3map = idx then print(" FOUND\n")
			
			else if #3map < idx then SearchNode(map, left, depth, path)
			
			else SearchNode(map, right, depth, path)
	);
	
(* For deleting nodes *)	
fun DeleteNode(map : int option * int option * int option, Empty, depth, path : int list, show : bool) = Empty |
	DeleteNode(map : int option * int option * int option, Node(idx, left, right, center), depth, path : int list, show : bool) =
	(
		if depth = 1 then
			if #1map = NONE then
			(
				let
					val pp = path @ [idx]
				in
					if show then
					(
						print("\n");
						printFullPath(pp);
						print(" deleted")
					)
					else ()
				end;
					
				AddNode((0, 0, 0), Empty, depth, path, show)
			)
			else
				let
					val m = Option.getOpt(#1map, 0);
					val n = #2map = NONE;
				in
					if m = idx andalso n then DeleteNode(map, Node(idx, left, right, center), depth + 1, path @ [idx], show)
					
					else if m = idx then Node(idx, left, right, DeleteNode(map, center, depth + 1, path @ [idx], show))
					
					else if m < idx then Node(idx, DeleteNode(map, left, depth, path, show), right, center)
			
					else Node(idx, left, DeleteNode(map, right, depth, path, show), center)
				end
		
		else if depth = 2 then
			if #2map = NONE then
			(
				let
					val pp = path @ [GetNodeIndex(center)]
				in
					if show then
					(
						print("\n");
						printFullPath(pp);
						print(" deleted")
					)
					else ()
				end;
				
				Node(idx, left, right, AddNode((Option.getOpt(#1map, 0), 0, 0), Empty, depth, path, show))
			)
			else
				let
					val m = Option.getOpt(#2map, 0);
					val n = #3map = NONE;
				in
					if m = idx andalso n then DeleteNode(map, Node(idx, left, right, center), depth + 1, path @ [idx], show)
					
					else if m = idx then Node(idx, left, right, DeleteNode(map, center, depth + 1, path @ [idx], show))
					
					else if m < idx then Node(idx, DeleteNode(map, left, depth, path, show), right, center)
			
					else Node(idx, left, DeleteNode(map, right, depth, path, show), center)
				end
		
		else
			if #3map = NONE then
			(
				let
					val pp = path @ [GetNodeIndex(center)]
				in
					if show then
					(
						print("\n");
						printFullPath(pp);
						print(" deleted")
					)
					else ()
				end;
				
				Node(idx, left, right, AddNode((Option.getOpt(#1map, 0), Option.getOpt(#2map, 0), 0), Empty, depth, path, show))
			)
			else
				let
					val m = Option.getOpt(#3map, 0);
				in
					if m = idx then Node(idx, left, right, Empty)
					
					else if m < idx then Node(idx, DeleteNode(map, left, depth, path, show), right, center)
			
					else Node(idx, left, DeleteNode(map, right, depth, path, show), center)
				end
	);

print("\n\n");
	
(* Generates node tree with random values *)
fun GENERATE(node : btree, num : int) =
		if num > 0 then
			GENERATE(AddNode((getRandInt(50), getRandInt(50), getRandInt(50)), node, 1, [], false), num - 1)
		else
			node;

(* User-friendly functions *)
fun ADD (map : int * int * int, node : btree) = AddNode(map, node, 1, [], true)
fun PRINT (node : btree) = printInorder(node, [])
fun SEARCH(map : int * int * int, node : btree) = SearchNode(map, node, 1, [])
fun DEL(map : string * string * string, node : btree) = DeleteNode((Int.fromString(#1map), Int.fromString(#2map), Int.fromString(#3map)), node, 1, [], true)