;;; For use in Lisp project
;;; Imported from project-data.lisp (M-x insert-file)

(defconstant message 
  '(
              no yes not yes yes not no no no is is hello yes no no yes not no
              not no hello is goodbye no yes when no yes not no not no
              no yes not is why why why yes is how is yes is why no yes not
              no no is is not no yes no yes hello yes hello yes no no yes
              no hello goodbye no is goodbye no yes when
    ))

;;; Imported from Dr. Wyatt make-freq-list.lisp and edited for comprehension (M-x insert-file)

;;; FUNCTION NAME:              freqlist
;;; DESCRIPTION:                create a list of a list of characters and freqency
;;; INPUT PARAMS:               "messVar" is the list of message;
;;; OUTPUT:                     list of sorted frequency of characters
(defun freqlist(messVar)
"Receives a message and passes it into another function to make frequency list"
(freqlist-recur messVar nil))

;;; FUNCTION NAME:		freqlist-recur
;;; DESCRIPTION:		cereates a frequency list recursively
;;; INPUT PARAMS:		"messVar" = message;
;;; OUTPUT:		       	frequency list and modified message
(defun freqlist-recur(messVar recurfreqlist)
"Receives a message and a frequency list, then appends to frequency list recursively"
(cond((endp messVar)recurfreqlist)
(T(freqlist-recur(rest messVar)(update(first messVar)recurfreqlist))))
)

;;; FUNCTION NAME:		update      
;;; DESCRIPTION:                update the frequency list	
;;; INPUT PARAMS:               "w" = current letter in the word; "freqlist-recur" = frequency list
;;; OUTPUT:		       	updated frequency list
(defun update (w freqlist-recur)
"each letter it receives, add it to the frequency list"
(cond((endp freqlist-recur)(list(list(list w) 1)))
((fequal w (first(first freqlist-recur))) (cons (incpair(first freqlist-recur))(rest freqlist-recur)))
(t(cons(first freqlist-recur)(update w (rest freqlist-recur)))))
)

;;; FUNCTION NAME:		fequal      
;;; DESCRIPTION:                ensure the inputted letter matches the 1st element in the list
;;; INPUT PARAMS:               "inputLetter" = a letter in the word; "freqlistVar" = frequency list
;;; OUTPUT:		       	T or Nil
(defun fequal (inputLetter freqlistVar)
"check if the inputted letter matches the 1st element in the list"
(equal inputLetter(first freqlistVar))
)

;;; FUNCTION NAME:	        incpair      
;;; DESCRIPTION:                increment the frequency of a letter
;;; INPUT PARAMS:               "ip" = frequency list pair
;;; OUTPUT:                     updated frequency of the letter
(defun incpair (ip)
"Increment the matching letter frequency by and return that list"
(list(first ip)(1+(second ip)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCTION NAME: 		htree-less
;;; DESCRIPTION:		returns T if weight of htree1 is less than htree2, else false
;;; INPUT PARAMS:		htree1 and htree2
;;; OUTPUT:    			displays T if weight of htree1 is less than htree2
(defun htree-less(htree1 htree2)
"Returns T if weight of htree1 is less than htree2, else false."
(<(htree-weight htree1)(htree-weight htree2)))

;;; FUNCTION NAME:		htree-symbols
;;; DESCRIPTION:		returns the list of symbols stored in the root of htree
;;; INPUT PARAMS:		htree - Huffman Tree
;;; OUTPUT:	       		the symbol of the root
(defun htree-symbols (htree)
"Returns the symbols stored in the root of the htree"
(first(root htree)))

;;; FUNCTION NAME:		htree-weight
;;; DESCRIPTION:		returns the weight of htree
;;; INPUT PARAMS:		htree is the htree
;;; OUTPUT:    			an integer value - the weight of the tree
(defun htree-weight(htree)
"Returns the weight of the tree"
(second(first htree)))

;;; FUNCTION NAME:		root
;;; DESCRIPTION:		returns the root of a htree
;;; INPUT PARAMS:		this is the root - Huffman Tree
;;; OUTPUT:	       		root of the htree
(defun root(htree)
"Returns the root of a htree"
(first htree))

;;; FUNCTION NAME:		htree-sort      
;;; DESCRIPTION:                sort the list of htree
;;; INPUT PARAMS:               "htree" is the list of htree
;;; OUTPUT:	       		returns the list of htrees sort by their weight
(defun htree-sort (htrees)
"sort the list of htrees in ascending order"
(sort(copy-seq htrees)#'htree-less))

;;; FUNCTION NAME:		create-huffman-tree
;;; DESCRIPTION:		creates a huffman tree from initial huffman tree from frequency list
;;; INPUT PARAMS:		initial Huffman Tree
;;; OUTPUT:		       	final Huffman Tree
(defun create-huffman-tree (htreelist)
"Creates a Huffman Tree with recursion"
(cond((eq (rest htreelist)nil)htreelist);if the rest of the list is nil, return the list
(T(create-huffman-tree (htree-sort (new-htree htreelist)))));else create a sorted new htree
)

;;; FUNCTION NAME:		new-htree
;;; DESCRIPTION:		merge 2 htrees' symbol and weight
;;; INPUT PARAMS:		messagein - Huffman Tree
;;; OUTPUT:		       	Huffman Tree
(defun new-htree(messagein)
"Returns a merged htree from a message"
(append(rest(rest messagein))(list(htree-merge(first messagein)(second messagein))))
)

;;; FUNCTION NAME:		htree-merge
;;; DESCRIPTION:		merge 2 htrees
;;; INPUT PARAMS:		htree1 and htree2 need to be merged
;;; OUTPUT:				return the Huffman Tree that results from merging the htree1 and htree2
(defun htree-merge (htree1 htree2)
"Returns the Huffman Tree that results from merging the htree1 and htree2"
(list(list(append (first(first htree1))(first(first htree2)))(+(htree-weight htree1)(htree-weight htree2)))htree1 htree2))

;;; FUNCTION NAME:		leaf-p
;;; DESCRIPTION:		test to see if Huffman Tree is a leaf
;;; INPUT PARAMS:		htree - Huffman Tree
;;; OUTPUT:				T or Nil
(defun leaf-p (htree)
"Returns true if htree is a leaf"
(null(left-subhtree htree)))

;;; FUNCTION NAME:		left-subtree
;;; DESCRIPTION:		returns the left subhtree of the htree
;;; INPUT PARAMS:		htree
;;; OUTPUT:				left subtree of an htree
(defun left-subhtree (htree)
"Returns the left sub-tree of a htree"
(second htree)
)

;;; FUNCTION NAME:		right-subhtree
;;; DESCRIPTION:		returns the right subhtree of the htree
;;; INPUT PARAMS:		htree
;;; OUTPUT:				right subtree of an htree
(defun right-subhtree (htree)
"Returns the right subtree of a htree"
(third htree)
)


;;; FUNCTION NAME:		make-huffman-tree      
;;; DESCRIPTION:        make huffman tree
;;; INPUT PARAMS:       "messVar" = inputted message
;;; OUTPUT:   			THE Huffman Tree
(defun make-huffman-tree (messVar)
"Creates a Huffman Tree from message, which is a list of symbols"
(first(create-huffman-tree(initial-htree messVar)))
)

;;; FUNCTION NAME:		initial-htree
;;; DESCRIPTION:		creates an initial htree from a frequency list
;;; INPUT PARAMS:		frequency list variable
;;; OUTPUT:				an initial htree and htreelist
(defun initial-htree(messfreqlist)
"Creates an initial Huffman Tree from the frequency list and an htree list"
(htree-sort(mapcar #'list (freqlist messfreqlist)));turn each list item into a list
)

;;; FUNCTION NAME: 		encode
;;; DESCRIPTION:		takes a htree and message then returns an encoded binary
;;; INPUT PARAMS:		htree-HuffmanTree & messVar-a message
;;; OUTPUT:				encoded binary
(defun encode(htree messVar)
"Encode message, using the HuffmanTree huffman-tree"
(cond((endp messVar)nil)	;if end of message, return nil
(t(append(encode-single htree(first messVar)'())(encode htree(rest messVar)))))	;else recursive call
)

;;; FUNCTION NAME:		encode-single
;;; DESCRIPTION:		based on the nodes of the tree, encode a htree
;;; INPUT PARAMS:		htree-HuffmanTree & letterVar-CharacterVariable & bm-BinaryMessage
;;; OUTPUT:				pass binary back to itself and the encode function
(defun encode-single(htree letterVar bm)
"Returns an encoded binary message by appending 0 or 1 to an input message"
(cond((leaf-p htree)bm)
((equal(pick-subtree htree letterVar)'0)(encode-single(left-subhtree htree)letterVar(append bm'(0))))
(t(equal(pick-subtree htree letterVar)'1)(encode-single(right-subhtree htree)letterVar(append bm'(1)))))
)

;;; FUNCTION NAME:		pick-subtree
;;; DESCRIPTION:		based on the charatcer in the subtree, it can go left or right
;;; INPUT PARAMS:		htree-HuffmanTree & letterVar-list of characters
;;; OUTPUT:				0 or 1 depends on which side of the tree the character exist in
(defun pick-subtree(htree letterVar)
"Go left or right"
(cond((equal letterVar(find letterVar(htree-symbols(left-subhtree htree))))0)
((equal letterVar(find letterVar(htree-symbols(right-subhtree htree))))1))
)

;;; FUNCTION NAME:		decode
;;; DESCRPTION:			decode a word
;;; INPUT PARAMS:		bl-binarylist to be decoded
;;; OUTPUT:				encoded message
(defun decode(bl htree)
"returns encoded message"
(decode-single bl htree htree nil)	;begin to decode
)

;;; FUNCTION NAME:		decode-single
;;; DESCRIPTION:		decodes a word
;;; INPUT PARAMS:		bl-binarylist & htree-HuffmanTree & curTree-current HuffmanTree
;;; OUTPUT:				decoded binary
(defun decode-single(bl htree curTree translation)
"Decodes a single word using decode-pick-subtree to decode message"
(cond((endp bl)translation)
(t(decode-pick-subtree bl curTree translation htree)))
)

;;; FUNCTION NAME:		decode-pick-subtree
;;; DESCRIPTION:		translate encoded message
;;; INPUT PARAMS:		bv-BinaryMessage & curTree-current htree & translation
;;; OUTPUT:				decoded message
(defun decode-pick-subtree(bv curTree translation htree)
(cond((leaf-p curTree)(decode-single bv htree htree(append translation(htree-symbols curTree))))
((equal(first bv)0)(decode-pick-subtree(rest bv)(left-subhtree curTree)translation htree))
(t(decode-pick-subtree(rest bv)(right-subhtree curTree)translation htree)))
)

