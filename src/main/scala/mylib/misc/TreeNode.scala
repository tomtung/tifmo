package mylib


package misc {
	
	abstract class TreeNode[E] {
		
		protected def core: Any
		
		protected def children: Iterable[_ <: (E, TreeNode[E])]
		
		protected def parent: (TreeNode[E], E)
		
	}
	
}
