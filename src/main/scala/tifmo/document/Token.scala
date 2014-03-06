package tifmo

import dcstree.WordBase
import dcstree.TokenBase

package document {
	
	class Token extends TokenBase with Serializable {
		
		var word = null:WordBase
		
		def getWord = word
		
		
		/**
		 * The surface string.
		 */
		var surface = null:String
		
		/**
		 * The coreference id (id of the mention cluster). `null` if not a mention.
		 */
		var corefID = null:String
		
		/**
		 * The id of the token. This id will be used to sort the children of a TokenNode.
		 */
		var id = -1
		
		var doc = null:Document
		
	}
}
