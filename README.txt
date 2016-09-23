GENERATE
========
- Description:
-	Generates n number of nodes for 3D-binary tree. Values are 1-50.
- Usage:
-	node = GENERATE(node, n)

ADD
===
- Description:
-	Adds 3-element tuple of integers to 3D-binary tree.
- Usage:
-	node = ADD((x,y,z), node)

DEL
===
- Description:
-	Deletes 3-element tuple of strings (interpreted as integers) from 3D-binary tree.
- Usage:
-	node = DEL(("x","y","_"), node) << Removes all sub nodes in y
-	node = DEL(("x","_","_"), node) << Removes all sub nodes in x
-	node = DEL(("_","_","_"), node) << Removes all all nodes

SEARCH
======
- Description:
-	Searches 3D-binary tree from 3-element tuple of integers input.
- Usage:
-	SEARCH((x,y,z), node)

PRINT
=====
- Description:
-	Displays all elements of 3D-binary tree in order.
- Usage:
-	PRINT(node)