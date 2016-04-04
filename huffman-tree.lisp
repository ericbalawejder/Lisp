;;; For use in Lisp project
;;; Imported from project-data.lisp (M-x insert-file)
(defconstant project-message 
  '(
              no yes not yes yes not no no no is is hello yes no no yes not no
              not no hello is goodbye no yes when no yes not no not no
              no yes not is why why why yes is how is yes is why no yes not
              no no is is not no yes no yes hello yes hello yes no no yes
              no hello goodbye no is goodbye no yes when
    ))

;;; From handout, for testing
(defconstant test-message
  '(
    A B C A D A E A F A G H A B A B A
    ))

;;; Imported from Dr. Wyatt make-freq-list.lisp and edited for comprehension
;;; (M-x insert-file)
;;; create a list of a list of characters and freqency value
(defun freqlist(message)
  "Receives a message and passes it into another function to make frequency list"
  (freqlist-recur message nil))

;;; creates a frequency list recursively
;;; returns a frequency list and modified message
(defun freqlist-recur(message recurfreqlist)
  "Receives a message and a frequency list, then appends to frequency list recursively"
  (cond((endp message)recurfreqlist)
       (T(freqlist-recur(rest message)(update(first message)recurfreqlist)))))
      
;;; update the frequency list value, "w" = current letter
;;; returns an updated frequency list
(defun update (w freqlist-recur)
  "Create a frquency list of words"
  (cond((endp freqlist-recur)(list(list(list w) 1)))
       ((fequal w (first(first freqlist-recur)))
	(cons (incpair(first freqlist-recur))(rest freqlist-recur)))
       (t(cons(first freqlist-recur)(update w (rest freqlist-recur))))))

;;; make sure the inputted letter matches the first element in the list
;;; "freqlistVar" = frequency list, return T 
(defun fequal (inputLetter freqlistVar)
  "check if the inputted letter matches the 1st element in the list"
  (equal inputLetter(first freqlistVar)))

;;; p = frequency list pair, update frequency of the letter
(defun incpair (p)
  "Increment the matching letter frequency and return that list"
  (list(first p)(1+(second p))))

;;; end make-freq-list.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns T if weight of htree1 is less than htree2
(defun htree-less(htree1 htree2)
  "Returns T if weight of htree1 is less than htree2"
  (<(htree-weight htree1)(htree-weight htree2)))

;;; returns the list of symbols stored in the root of htree
(defun htree-symbols (htree)
  "Returns the symbols stored in the root of the htree"
  (first(root htree)))

;;; returns the weight of htree (integer)
(defun htree-weight(htree)
  "Returns the weight of the tree"
  (second(first htree)))

;;; returns the root of a htree
(defun root(htree)
  "Returns the root of a htree"
  (first htree))

;;; returns the list of htrees sorted by their weight
(defun htree-sort (htrees)
  "sort the list of htrees in ascending order"
  (sort(copy-seq htrees)#'htree-less))

;;; creates a huffman tree from initial huffman tree from frequency list
(defun create-huffman-tree (htreelist)
  "Creates a huffman tree with recursion"
  (cond((eq (rest htreelist)nil)htreelist)
       (T(create-huffman-tree (htree-sort (new-htree htreelist))))))

;;; merge 2 htrees' symbol and weight, return a huffman tree
(defun new-htree(messagein)
  "Returns a merged htree from a message"
  (append(rest(rest messagein))(list(htree-merge(first messagein)(second messagein)))))

;;; return the huffman tree that results from merging the htree1 and htree2
(defun htree-merge (htree1 htree2)
  "Returns the huffman tree that results from merging the htree1 and htree2"
  (list(list(append (first(first htree1))(first(first htree2)))
	    (+(htree-weight htree1)(htree-weight htree2)))htree1 htree2))

;;; test to see if huffman tree is a leaf, return T 
(defun leaf-p (htree)
  "Returns true if htree is a leaf"
  (null(left-subhtree htree)))

;;; returns left subtree of an htree
(defun left-subhtree (htree)
  "Returns the left sub-tree of a htree"
  (second htree))

;;; return right subtree of an htree
(defun right-subhtree (htree)
  "Returns the right subtree of a htree"
  (third htree))

;;; return the huffman tree from project-message
;;; made from create-huffman-tree 
(defun make-huffman-tree (message)
  "Creates a huffman tree from message"
  (first(create-huffman-tree(initial-htree message))))

;;; return an initial htree and htreelist
(defun initial-htree(messfreqlist)
  "Creates an initial huffman tree from the frequency list and an htree list.
   Turn each list item into a list"
  (htree-sort(mapcar #'list (freqlist messfreqlist))))

;;; returns encoded binary of a huffman tree and a message
(defun encode(htree message)
  "Encode message, using the huffman tree"
  (cond((endp message)nil)	
       (t(append(encode-single htree(first message)'())(encode htree(rest message))))))

;;; based on the nodes of the tree, encode a htree
;;; htree-huffman tree, letterVar-character variable, bm-binary message
;;; pass binary back to itself and the encode function
(defun encode-single(htree letterVar bm)
  "Returns an encoded binary message by appending 0 or 1 to an input message"
  (cond((leaf-p htree)bm)
       ((equal(pick-subtree htree letterVar)'0)
	(encode-single(left-subhtree htree)letterVar(append bm'(0))))
       (t(equal(pick-subtree htree letterVar)'1)
	 (encode-single(right-subhtree htree)letterVar(append bm'(1))))))

;;; take in a huffman tree and a list of characters
;;; return 0 to branch left and 1 to branch right
(defun pick-subtree(htree letterVar)
  "left branch = 0, right branch = 1"
  (cond((equal letterVar(find letterVar(htree-symbols(left-subhtree htree))))0)
       ((equal letterVar(find letterVar(htree-symbols(right-subhtree htree))))1)))

;;; decode a word, bl-binarylist to be decoded, returns encoded message
(defun decode(bl htree)
  "returns encoded message"
  (decode-single bl htree htree nil))  

;;; bl-binarylist, htree-huffman tree, curTree-current huffman tree
;;; returns decoded binary
(defun decode-single(bl htree curTree translation)
  "Decodes a single word using decode-pick-subtree to decode message"
  (cond((endp bl)translation)
       (t(decode-pick-subtree bl curTree translation htree))))

;;; bv-binary message, curTree-current htree, translation
;;; return decoded message
(defun decode-pick-subtree(bv curTree translation htree)
  "Translate encoded message"
  (cond((leaf-p curTree)
	(decode-single bv htree htree(append translation(htree-symbols curTree))))
       ((equal(first bv)0)
	(decode-pick-subtree(rest bv)(left-subhtree curTree)translation htree))
       (t(decode-pick-subtree(rest bv)(right-subhtree curTree)translation htree))))

