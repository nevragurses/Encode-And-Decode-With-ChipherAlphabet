; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************


;STUDENT NEVRA GURSES
;NO:161044071


;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"


; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
(defun read-as-list (filename)
	;smallList is used for creating word as list of characters,biglist is used for creating word list (paragraph).
	(let ( (file (open filename :if-does-not-exist nil)) (smallList (list  )) (bigList (list ))) 
		(when file
			(loop for letter = (read-char file nil) ;loop until document ends.
			    while letter 
					do 
					(when (or (equal letter #\Space)(equal letter  #\NewLine) )
						(setq bigList (append bigList (list smallList))) ;when current character in document is space or newLine,word as character list is appended in word list(paragraph).
						(setq smallList nil); word as character list is deleted.
					)
					(when (and (not(equal letter #\Space ))  (not(equal letter  #\NewLine)) );when current character is not space or newLine;
						(setq smallList (append smallList (list letter))) ;current character is appended in  list of characters  that is named smallList. 
					)
			)
			(setq bigList (append bigList (list smallList))) ; this is for recording last word  in word list(paragraph).
		)
		(close file)
		bigList ;returns list of words.
		
	)

)


;; -----------------------------------------------------
;; HELPERS

;This function creates random letters and map them with plain alphabet.So a chipher alphabet is created with hash table.
(defun chipher ()
	(let ( (lst (list ) )   (encodeTable  ( make-hash-table ))  )
		(setf lst (make-list 26 :initial-element -1)) ;list for alphabet.
		(write-Line "One to one mapping for plain alphabet and chipher alphabet :")
		(loop for i from 0 to 25 
				do 	
					(setq rdm (+ 0 (random 26)) )  
					( when (not(equal (position rdm lst :test #'equal) nil)) ;when before this random is used;
						(loop ;new random is taken until chipher alphabet does not contain them.
							(setq rdm (+ 0 (random 26)))  
							(when  (equal (position rdm lst :test #'equal) nil) (return rdm))
						)
					)	
					(format t " key: ~d  value:~d "  (i2c i) (i2c rdm)) (terpri) ;prints one to one mapping of plain and chipher alphabet in screen.
					(setf (nth i lst ) rdm) 
					(setf (gethash  (i2c i) encodeTable ) (i2c rdm)) ;creates one to one mapping by using hash table.
		)
		encodeTable ;return one to one mapping of plain and chipher alphabet as hash table.
	)	
)
;This function encodes a word according to given alphabet.
(defun encodeWord (word cipherAlph)
	(let ((listEncode (list )) )
		(setf listEncode (make-list (length word) :initial-element -1)) ;list for encoded word.
		(loop for k from 0 to (-( length word) 1)
			do
				(setf (nth k listEncode ) (gethash (char word k) cipherAlph ) )	;gets value of letter in alphabet that is map,and assigns it in encode list.
		)
		listEncode ;return encoded word.
	)
)
;This function encodes all document.It uses encodeWord function.
(defun encodeDocument (paragraph)
	(let ((strList (list )) (myEncodedList (list )) (randomAlph( make-hash-table)))
		(setf strlist (make-list (list-length paragraph) :initial-element 0))
		(setf myEncodedList (make-list (list-length paragraph) :initial-element 0))
		(loop for i from 0 to (-( list-length paragraph) 1)
				do 	
					(setf (nth i strlist) (coerce (nth i paragraph) 'string)) ;converts words that is list of characters to string. 				
		)
		(setq randomAlph (chipher) ) ;creates a random  one to one mapping alphabet by calling chipher function.
		(loop for j from 0 to (-(list-length strlist)1)
			do
				(setf (nth j myEncodedList) (encodeWord  (nth j strlist) randomAlph));endodes a word and adds it encode document list.
		)
		myEncodedList ;returns encoded document.
	)		
)
;This function for calculating unrepeated letter number of a word.This is the helper function to gen decoders.
(defun letterNum(word)
	(let ( (lst (list )) (num 0) )
		(setf lst (make-list 26 :initial-element '0))
		(loop for i from 0 to (-( list-length word) 1)
			do 	
					(setf (nth (c2i  (nth i word) ) lst)  (+(nth (c2i (nth i word))  lst) 1));all letters is taken a list according to c2i indexes.And number in that index increases.
		)
		(loop for z from 0 to (-( list-length lst) 1) ;this loop for calculating unrepeated letter number.
			do	
					(when (not (equal(nth z lst) 0))
						(setq num (+ num 1))
					)	
		)
		num ;return unrepeated letter number of a word.
	)	
)
;This function for creating permutation list of given list elements.This is the helper functions for gen-decoders.
(defun permutation (list)
    (if (null list) '(())
  		(mapcan #'  (lambda (x)
    					(mapcar #'(lambda (y) (cons x y))
     	 					(permutation (remove x list :count 1)));recursive call.
				    ) list
		)
	)
)
;This function creates list that is number of uses of letters  according to 
;how many times a letter is used in the word.Numbers are replaced according to c2i indexes.This function helper for gen-decoders. 
(defun word (word )
	(let ((lst (list )))
		(setq lst (make-list 26 :initial-element 0))
		(loop for m from 0 to (-( list-length word ) 1)
			do 	
				(setf (nth (c2i  (nth m word) ) lst)  (+(nth (c2i (nth m word))  lst) 1));all letters is replaced list index according to c2i and number increases that index.		
		)
		lst
	)	
	
)
;This function for creating combination list according to given number and given list.This is the helper functions for gen-decoders.
(defun combination (num list )
	(let ((listCombination (list )))  
		(labels ((firstCombination (lst i num)
			(when (>= (length lst) num)
				(if (zerop num) (return-from firstCombination (setq listCombination (append listCombination (list i))) ))
				(firstCombination (cdr lst) i num)
				(firstCombination (cdr lst) (cons (first lst) i) (1- num)))))
			(firstCombination list nil num)
		)
		listCombination ;return combination list.
	)	
)
;This function creates word that is list of characters without unrepeated letters.It uses  list that is number of uses of letters.
;This function is helper for gen-decoders.
(defun wordWithoutUnrepeated (word lst)
	(let ((newWord (list )) )  
		(loop for k from 0 to (-( list-length word ) 1)
			do
				(when (equal  (nth  (c2i (nth k word)) lst) 1 ) ;If letter is unrepeated.
					(setq newWord (append newWord (list  (nth k word)))) 
				)
				(when  (and (not (equal  (nth  (c2i (nth k word))lst) 1 ))(equal (position (nth k word) newWord) nil  ));If letter is repeated,One of them added newWord.
					(setq newWord (append newWord (list  (nth k word)))) 
				)
		)	
		newWord ;return unrepeated word that is list of characters.
	)	
)

;This function is for finding the most common 6 letters of paragraph.
;This function helper function for Frequency Analysis Gen_Decoder_B functions.	
(defun frequentLetter(paragraph)
	(let ( (lst (list )) (frequent (list )) (tempList (list ))   )
		(setf lst (make-list 26 :initial-element '0)) ;for keeping  number of uses of letters.
		(setf frequent (make-list 26 :initial-element -1))
		(setf tempList (make-list 6 :initial-element '0))

		(loop for i from 0 to (-( list-length paragraph) 1)
			do 	
				(loop for j from 0 to (-( list-length (nth i paragraph)) 1 )
					do
						(setf (nth (c2i (nth j (nth i paragraph)) ) lst)  (+(nth (c2i (nth j (nth i paragraph)) ) lst) 1));Each letter is taken a list according to c2i and number is that index increases.
				)	
		)
		(loop for z from 0 to 5 ;this loop for finding the most common 6 letters.
 			do	
				(setf (nth z tempList ) (i2c ( position (apply 'max lst) lst))) ;the common letter is taken a list according to c2i.
				(setf  (nth (position (apply 'max lst) lst ) lst) 0 );the common letter deleted for calculation of next steps.	 
		)
		(write "In order of most frequent 6 letter in document is :")
		(print tempList) (terpri)(terpri)
		;These operations for assigning letters in homework according to most common 6 letter in list according to c2i index.
		(setf (nth (c2i (nth 0 tempList)) frequent ) #\e)
		(setf (nth (c2i (nth 1 tempList)) frequent ) #\t)
		(setf (nth (c2i (nth 2 tempList) ) frequent ) #\a)
		(setf (nth (c2i (nth 3 tempList)) frequent ) #\o)
		(setf (nth (c2i (nth 4 tempList)) frequent ) #\i)
		(setf (nth (c2i (nth 5 tempList)) frequent ) #\n)


		frequent ;returns  most common 6 letter list.
	)	
)
;This function creating random chipher alphabet according to frequency analysis.Takes list of the most common 6 letter as parameter.
(defun randomAccordFrequent (frequentList)
	(let ( (freqHash ( make-hash-table )) (tempFreq(list )) )
		(setf tempFreq (make-list 26 :initial-element -1))
		(loop for i from 0 to (-( list-length frequentList) 1)
			do	
				(setq randomFreq (+ 0 (random 26)) ) 
				( when (equal (nth i frequentList) -1 )
					(when (not(equal (position (i2c randomFreq) frequentList :test #'equal) nil)) ;When this random number is in list.
						(loop
							(setq randomFreq (+ 0 (random 26))) ;new random number is taken. 
							(when  (equal (position ( i2c randomFreq) frequentList :test #'equal) nil) (return randomFreq));random number added when it is not in list.
						)
					)
					(setf (nth i frequentList ) (i2c randomFreq));random number added chipher alphabet after converted character.
				)
		)	
		(loop for j from 0 to 25
			do
				(setf (gethash  (i2c j) freqHash ) (nth j frequentList)) ;hash table is created according to plain alphabet and chipher alphabet.
				(terpri)
				(format t " key: ~d  value:~d "  (i2c j) (nth j frequentList)) (terpri)
		)	
		freqHash ;return hash table
	)
)
;This function to create hash table of all words in dictionary.Helper function for spell-checker-1.
(defun helperForSpellChecker-1 (filename )
		;open file for recording all words in hash table by using hash code.
		(let ( (file (open filename :if-does-not-exist nil)) (hashTable (make-hash-table )) )
			(when file
				(loop for word = (read-line file nil)
					while word 
						do
							(setf (gethash  (sxhash word ) hashTable ) word) ;records current word in hash table.Key value of hash table is hash code.
				)	
			)
			(close file)
			hashTable ;returns created hash table.
		)
)	
;This function to check whether a word is in dictionary or not  by using brute force version.
(defun spell-checker-0 (word filename)
	(let ( (file (open filename :if-does-not-exist nil)) ) ;opening file for reading.
		(when file
			(loop for wordIn = (read-line file nil)
			    while wordIn
					do 
					(when ( equal wordIn word) ;when current word in dictionary is equal to searhing word;
						(return-from spell-checker-0 t) ;return true.
					)
			)
		)
		(close file)
	)
	(return-from spell-checker-0 nil) ;if it doesn't found,return false.
)
;This function for finding a word in dictionary by using hash mapping.
;this function takes hashTable as parameter,I use helper function to create hash table of dictionary before calling this function.
;I don't do creating hash table is in this function because in gen-decoder functions,
;when calling this function in loop ,creating hash table takes much time
;so I used helper function to create hashTable and before using this spell-checker,I call them where I use this function.
(defun spell-checker-1 (hashTable word) 
 	(if ( equal (gethash (sxhash word ) hashTable ) word) ;controls searhing word is in hashTable or not.
			t 
		nil
	)	
)


;; -----------------------------------------------------
;; DECODE FUNCTIONS

;This function decodes encoded document by using brute force.
(defun Gen-Decoder-A (paragraph)
	(let ((outmap ( make-hash-table)) (decoder (list )) (alphabet (list ) )(wordNumber (list ))(combinationList (list)) (lst ( list))(newWord (list ))(permutationTemp(list ))  )                          
		(setq  generalHashTable ( helperForSpellChecker-1 "dictionary2.txt") ) ;creating hash table of all words in paragraph,this is used as helper function for spell-checker-1. 
		(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t  #\u  #\v  #\w  #\x #\y #\z) ); creating alphabet for taking permutations and combinations.
		(loop for i from 0 to   (-( list-length paragraph )1 )
			do
				(setq wordNumber (letterNum(nth i paragraph))) ;the number of letters in word as unrepeated.
				(setq combinationList (combination wordNumber alphabet) ) ; all combination list of alphabet according to unrepeted letter number.
				(setq lst (word (nth i paragraph))) ; list that is number of uses of letters.
				(setq newWord (wordWithoutUnrepeated (nth i paragraph) lst));This function creates word that is list of characters without unrepeated letters.
				(loop for j from 0 to (-( list-length combinationList) 1);loop for combination list of  all words in document.
					do
						(setq permutationTemp (permutation (nth j combinationList))) ;permutation list of nth combination list.
						;alpHash is a hash table to keep  general mapping according to permutation.
						(let ( (alpHash (make-hash-table )) (newPermutation (list ))   ) 
							(loop for n from 0 to    (-(list-length permutationTemp)1); loop for all permutations of a combination.
								do
									(loop for z from 0 to  (- wordNumber 1) ;this loop for creating inner mapping.
										do
										(when ( equal(gethash (nth z newWord) outMap)nil) ;if letter is not in big hash table;
												(setf (gethash (nth z newWord) alpHash ) (nth z (nth n permutationTemp ) ))	;assign nth term of permutation in inner hash table.
										)	
										(when (not ( equal(gethash (nth z newWord) outMap)nil) ) ;if letter in the big hash table ,this letter has a map,copy it inner hash table.
											(setf (gethash (nth z newWord) alpHash )(gethash (nth z newWord) outMap) )																			
										)	
									)				
									(setq newPermutation (list)) ;for creating word according to occured mapping.
									(loop for l from 0 to (-( list-length (nth i paragraph) ) 1)
										do
											(setq newPermutation  (append newPermutation (list  (gethash (nth l (nth i paragraph))alpHash))) )
									)
									(when (not(equal (spell-checker-1 generalHashTable (coerce newPermutation 'string) ) nil)) ;if that word is in dictionary;
										(loop for m from 0 to (- wordNumber 1)
											do
												(when (equal  (gethash (nth m newWord) outMap )nil )
													(setf (gethash (nth m newWord) outMap ) (gethash (nth m newWord) alpHash )  ) ;inner map transform into outer map.
												)	
											)
										(loop for value being each hash-value of outMap
											do
												(setq alphabet (remove value alphabet)) ;mapping letters is removed alphabet because they are not using after when permutation taken.
										)
										(setq j ( list-length combinationList)) ;turn into combination loop.
										(setq n (list-length permutationTemp));turn into permutation loop.
										(setq decoder (append decoder (list  newPermutation)))	;appends finded words into decoder list.
									)
							)
						)
					)	
		)
		decoder
		
	)
				
)
;This function decodes encoded document by using frecuency analysis.
(defun Gen-Decoder-B-0 (paragraph)
  	(let ((outmap ( make-hash-table)) (decoder (list )) (alphabet (list ) )(wordNumber (list ))(combinationList (list)) (lst ( list))(newWord (list ))(permutationTemp(list ))  )                          
		(setq  generalHashTable ( helperForSpellChecker-1 "dictionary2.txt") ) ;creating hash table of all words in paragraph,this is used as helper function for spell-checker-1. 
		(setq alphabet '(#\b #\c #\d  #\f #\g #\h  #\j #\k #\l #\m   #\p #\q #\r #\s   #\u  #\v  #\w  #\x #\y #\z) ); creating alphabet for taking permutations and combinations.The common 6 letter is deleted because because they are spesific.
		(setq frequentLst  (frequentLetter paragraph )) ;finds common 6 letter in paragraph calling helper function.
		(loop for w from 0 to  (-( list-length frequentLst) 1) ;this loop for assign mapping in general hash table according to common 6 letter.
			do
				(when (not ( equal(nth w frequentLst)-1))
					(setf (gethash (i2c w) outMap )(nth w frequentLst ))	
				)
		)
		(loop for i from 0 to   (-( list-length paragraph )1 )
			do
				(setq wordNumber (letterNum(nth i paragraph))) ;the number of letters in word as unrepeated.
				(setq combinationList (combination wordNumber alphabet) ) ; all combination list of alphabet according to unrepeted letter number.
				(setq lst (word (nth i paragraph))) ; list that is number of uses of letters.
				(setq newWord (wordWithoutUnrepeated (nth i paragraph) lst));This function creates word that is list of characters without unrepeated letters.
				(loop for j from 0 to (-( list-length combinationList) 1);loop for combination list of  all words in document.
					do
						(setq permutationTemp (permutation (nth j combinationList))) ;permutation list of nth combination list.
						;alpHash is a hash table to keep  general mapping according to permutation.
						(let ( (alpHash (make-hash-table )) (newPermutation (list ))   ) 
							(loop for n from 0 to    (-(list-length permutationTemp)1); loop for all permutations of a combination.
								do
									(loop for z from 0 to  (- wordNumber 1) ;this loop for creating inner mapping.
										do
										(when ( equal(gethash (nth z newWord) outMap)nil) ;if letter is not in big hash table;
												(setf (gethash (nth z newWord) alpHash ) (nth z (nth n permutationTemp ) ))	;assign nth term of permutation in inner hash table.
										)	
										(when (not ( equal(gethash (nth z newWord) outMap)nil) ) ;if letter in the big hash table ,this letter has a map,copy it inner hash table.
											(setf (gethash (nth z newWord) alpHash )(gethash (nth z newWord) outMap) )																			
										)	
									)				
									(setq newPermutation (list)) ;for creating word according to occured mapping.
									(loop for l from 0 to (-( list-length (nth i paragraph) ) 1)
										do
											(setq newPermutation  (append newPermutation (list  (gethash (nth l (nth i paragraph))alpHash))) )
									)
									(when (not(equal (spell-checker-1 generalHashTable (coerce newPermutation 'string) ) nil)) ;if that word is in dictionary;
										(loop for m from 0 to (- wordNumber 1)
											do
												(when (equal  (gethash (nth m newWord) outMap )nil )
													(setf (gethash (nth m newWord) outMap ) (gethash (nth m newWord) alpHash )  ) ;inner map transform into outer map.
												)	
											)
										(loop for value being each hash-value of outMap
											do
												(setq alphabet (remove value alphabet)) ;mapping letters is removed alphabet because they are not using after when permutation taken.
										)
										(setq j ( list-length combinationList)) ;turn into combination loop.
										(setq n (list-length permutationTemp)) ;turn into permutation loop.
										(setq decoder (append decoder (list  newPermutation)))	;appends finded words into decoder list.
									)
							)
						)
					)	
		)
		decoder
		
	)
				
)

 ;This function takes an encoded document and a decoding function as input, and returns the entire document in plain text. 
(defun Code-Breaker (document decoder)
  	( funcall decoder document ) ;calling decoder function.
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")(terpri)(terpri)
	(let (doc (list ))
		(write-line "--------TESTING READ-AS-LIST FUNCTION------------")
		(write-line "****READING DOCUMENT2.TXT FILE AND RECORDING WORDS THAT IS LIST OF CHARACTERS IN LIST****")
		(setq doc (read-as-list "document2.txt"))  
		(print doc) (terpri)(terpri)
		(write-line "--------TESTING SPELL-CHECKER-0 FUNCTION------------")
		(write-line "****  Searching word that is 'pretty' in dictionary2  ****")
		(print (spell-checker-0 "pretty" "dictionary2.txt" ))(terpri)(terpri)
		(write-line "****  Searching word that is 'nevra' in dictionary2  ****")
		(print (spell-checker-0 "nevra" "dictionary2.txt" ))(terpri)(terpri) ;nevra is not in dictionary,the result must be nil.
		(write-line "--------TESTING SPELL-CHECKER-1 FUNCTION------------")
		(write-line "****  Searching word that is 'whisper' in dictionary2  ****")
		(write (spell-checker-1 (helperForSpellChecker-1 "dictionary2.txt") "whisper" ));when calling spell checker-1, I created hash table of dictionary with helper function.
		(terpri)(terpri)
		(write-line "--------TESTING FINDING THE MOST FREQUENT 6 LETTER IN DOCUMENT FUNCTION: ------------")
		(frequentLetter doc)
		(write-line "--------TESTING ENCODING FOR DOCUMENT FUNCTION------------")
		(write-line "****  Document1.txt file is encoding with a chipher alphabet,this chipher alphabet and result is showing..  ****")(terpri)
		(write "  Document1.txt: ")
		(write (read-as-list "document1.txt"))(terpri)
		(write-line "After Encoding...")(terpri)
		(write (encodeDocument (read-as-list "document1.txt") ))
		(terpri)(terpri)
		(write-line "--------TESTING GEN DECODER-A FUNCTION: ------------")
		(write-line "****  Firstly MyDocument.txt file encoding...  ****")(terpri)
		(write "Before encoding myDocument.txt file is : ")
		(write (read-as-list "myDocument.txt"))(terpri)(terpri)
		(setq list (encodeDocument (read-as-list "myDocument.txt")))(terpri)
		(write "After encoding,encoded document is: ")
		(write list)(terpri)(terpri)
		(write-line "****  GEN-DECODER-A IS WORKING TO SOLVE ENCODED DOCUMENT  ****")(terpri)
		(write (Gen-Decoder-A list)) (terpri)(terpri)
		(write-line "--------TESTING GEN DECODER-B-0 FUNCTION: ------------")
		(write-line "****  Firstly MyDocument2.txt file encoding...  ****")(terpri)
		(setq lst (encodeDocument(read-as-list "myDocument2.txt")))
		(write "After encoding,encoded document is: ")
		(write lst)(terpri)(terpri)
		(write-line "****  GEN-DECODER-B-0 IS WORKING TO SOLVE ENCODED DOCUMENT  ****")(terpri)
		(write (Gen-Decoder-B-0 lst))(terpri)(terpri)

		(write-line "--------TESTING CODE-BREAKER FUNCTION: ------------")
		(write-line "****  Firstly MyDocument.txt file encoding...  ****")(terpri)
		(write "Before encoding myDocument.txt file is : ")
		(write (read-as-list "myDocument.txt"))(terpri)(terpri)
		(write "After encoding,encoded document is: ")
		(write list)(terpri)(terpri)
		(write-line "****  CODE BREAKER IS WORKING TO SOLVE ENCODED DOCUMENT  ****")(terpri)
		(write (code-Breaker list #'Gen-Decoder-A  ))(terpri)
		
	)
)
(test_on_test_data)