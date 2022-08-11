Class {
	#name : #DartScanner,
	#superclass : #SmaCCScanner,
	#category : #'SmaCC_Dart'
}

{ #category : #'generated-initialization' }
DartScanner class >> initializeKeywordMap [

	keywordMap := Dictionary new.
	#( #( 140 'Function' 95 ) #( 140 'abstract' 86 )
	   #( 140 'as' 87 ) #( 140 'assert' 53 ) #( 140 'async' 111 )
	   #( 140 'await' 109 ) #( 140 'break' 54 ) #( 140 'case' 55 )
	   #( 140 'catch' 56 ) #( 140 'class' 57 ) #( 140 'const' 58 )
	   #( 140 'continue' 59 ) #( 140 'covariant' 88 )
	   #( 140 'default' 60 ) #( 140 'deferred' 89 )
	   #( 140 'do' 61 ) #( 140 'dynamic' 90 ) #( 140 'else' 62 )
	   #( 140 'enum' 63 ) #( 140 'export' 91 ) #( 140 'extends' 64 )
	   #( 140 'extension' 92 ) #( 140 'external' 93 )
	   #( 140 'factory' 94 ) #( 140 'false' 65 ) #( 140 'final' 66 )
	   #( 140 'finally' 67 ) #( 140 'for' 68 ) #( 140 'get' 96 )
	   #( 140 'hide' 112 ) #( 140 'if' 69 ) #( 140 'implements'
	      97 ) #( 140 'import' 98 ) #( 140 'in' 70 ) #( 140 'interface'
	      99 ) #( 140 'is' 71 ) #( 140 'late' 100 ) #( 140 'library'
	      101 ) #( 140 'mixin' 103 ) #( 140 'new' 72 ) #( 140 'null'
	      73 ) #( 140 'of' 113 ) #( 140 'on' 114 ) #( 140 'operator'
	      102 ) #( 140 'part' 104 ) #( 140 'required' 105 )
	   #( 140 'rethrow' 74 ) #( 140 'return' 75 ) #( 140 'set' 106 )
	   #( 140 'show' 115 ) #( 140 'static' 107 ) #( 140 'super' 76 )
	   #( 140 'switch' 77 ) #( 140 'sync' 116 ) #( 140 'this' 78 )
	   #( 140 'throw' 79 ) #( 140 'true' 80 ) #( 140 'try' 81 )
	   #( 140 'typedef' 108 ) #( 140 'var' 82 ) #( 140 'void' 83 )
	   #( 140 'while' 84 ) #( 140 'with' 85 ) #( 140 'yield' 110 ) ) do: [ 
		:each | 
		(keywordMap at: each first ifAbsentPut: [ Dictionary new ])
			at: (each at: 2)
			put: each last ].
	^ keywordMap
]

{ #category : #generated }
DartScanner >> emptySymbolTokenId [

	^ 454
]

{ #category : #generated }
DartScanner >> errorTokenId [

	^ 455
]

{ #category : #generated }
DartScanner >> scan1 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 125 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan2 ].
	currentCharacter == $\ ifTrue: [ ^ self scan3 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan10 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ ^ self scan5 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan11 [

	[ 
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan12 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 121 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan13 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan12 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 122 ) ].
	^ self scan12X1
]

{ #category : #generated }
DartScanner >> scan12X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $$ ifTrue: [ ^ self scan12 ].
			currentCharacter == $' ifTrue: [ 
				^ self recordAndReportMatch: #( 121 ) ].
			currentCharacter == $\ ifTrue: [ ^ self scan13 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan11 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan13 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan11 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan11 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan11 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan11 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan11 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan14 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan14 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan11 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan14 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan11 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan15 [

	self step.
	currentCharacter == $' ifTrue: [ ^ self scan16 ].
	currentCharacter == $| ifTrue: [ ^ self scan17 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan16 [

	self step.
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan18 ].
		currentCharacter == $' ifTrue: [ 
			^ self recordAndReportMatch: #( 129 ) ].
		currentCharacter == $\ ifTrue: [ ^ self scan19 ].
		^ self scan15 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan17 [

	self step.
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan18 ].
		currentCharacter == $\ ifTrue: [ ^ self scan19 ].
		(currentCharacter <= $& or: [ currentCharacter >= $( ]) ifTrue: [ 
			^ self scan15 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan18 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 130 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $' ifTrue: [ ^ self scan16 ].
			currentCharacter == $| ifTrue: [ ^ self scan17 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan19 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan15 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan15 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan15 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan15 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan15 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan20 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan20 ].
	^ self scan15
]

{ #category : #generated }
DartScanner >> scan2 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	^ self scan2X1
]

{ #category : #generated }
DartScanner >> scan20 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan15 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan21 [

	[ 
	self recordMatch: #( 117 ).
	self step.
	currentCharacter isDigit ] whileTrue.
	('Ee' includes: currentCharacter) ifTrue: [ ^ self scan22 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan22 [

	self step.
	currentCharacter isDigit ifTrue: [ 
		[ 
		self recordMatch: #( 117 ).
		self step.
		currentCharacter isDigit ] whileTrue.
		^ self reportLastMatch ].
	('+-' includes: currentCharacter) ifTrue: [ 
		self step.
		currentCharacter isDigit ifTrue: [ 
			[ 
			self recordMatch: #( 117 ).
			self step.
			currentCharacter isDigit ] whileTrue ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan23 [

	self step.
	currentCharacter isDigit ifTrue: [ ^ self scan21 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan24 [

	[ 
	self recordMatch: #( 117 ).
	self step.
	currentCharacter == $. ifTrue: [ ^ self scan23 ].
	currentCharacter isDigit ] whileTrue.
	('Ee' includes: currentCharacter) ifTrue: [ ^ self scan22 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25 [

	self recordMatch: #( 36 ).
	self step.
	currentCharacter == $. ifTrue: [ ^ self scan25X5 ].
	^ self scan25X6
]

{ #category : #generated }
DartScanner >> scan25X1 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan25X1X2 ].
	currentCharacter == $$ ifTrue: [ ^ self scan32 ].
	currentCharacter == $' ifTrue: [ ^ self scan25X1X1 ].
	currentCharacter == $\ ifTrue: [ ^ self scan38 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan26 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25X1X1 [

	self recordMatch: #( 124 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan64 ].
	currentCharacter == $$ ifTrue: [ ^ self scan65 ].
	currentCharacter == $' ifTrue: [ ^ self scan34 ].
	currentCharacter == $\ ifTrue: [ ^ self scan66 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan58 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scan25X1X2 [

	self recordMatch: #( 128 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan28 ].
	currentCharacter == $$ ifTrue: [ ^ self scan50 ].
	currentCharacter == $' ifTrue: [ ^ self scan51 ].
	currentCharacter == $\ ifTrue: [ ^ self scan52 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan44 ].
	^ self scan41
]

{ #category : #generated }
DartScanner >> scan25X2 [

	self recordMatch: #( 124 ).
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $' ifTrue: [ ^ self scan54 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25X3 [

	self recordMatch: #( 128 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan40 ].
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25X4 [

	self recordMatch: #( 39 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 40 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25X5 [

	self recordMatch: #( 37 ).
	self step.
	currentCharacter == $. ifTrue: [ 
		^ self recordAndReportMatch: #( 38 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25X6 [

	currentCharacter == $? ifTrue: [ ^ self scan25X4 ].
	currentCharacter == $} ifTrue: [ 
		self recordMatch: #( 138 ).
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan25X3 ].
		currentCharacter == $$ ifTrue: [ ^ self scan32 ].
		currentCharacter == $' ifTrue: [ ^ self scan25X2 ].
		currentCharacter == $\ ifTrue: [ ^ self scan38 ].
		currentCharacter == $| ifTrue: [ ^ self scan25X1 ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			^ self scan26 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan26 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan27 ].
	currentCharacter == $$ ifTrue: [ ^ self scan32 ].
	currentCharacter == $' ifTrue: [ ^ self scan33 ].
	currentCharacter == $\ ifTrue: [ ^ self scan38 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan27 [

	self recordMatch: #( 128 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan28 [

	[ 
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan29 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 123 ) ].
	^ self scan29X1
]

{ #category : #generated }
DartScanner >> scan29X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $$ ifTrue: [ ^ self scan29 ].
			currentCharacter == $' ifTrue: [ 
				^ self recordAndReportMatch: #( 124 ) ].
			currentCharacter == $\ ifTrue: [ ^ self scan30 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan28 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan2X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ 
				^ self recordAndReportMatch: #( 125 ) ].
			currentCharacter == $$ ifTrue: [ ^ self scan2 ].
			currentCharacter == $\ ifTrue: [ ^ self scan3 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan1 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan3 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan1 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan1 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan1 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan1 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan1 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan4 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan4 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan1 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan30 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan28 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan28 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan28 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan28 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan28 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan31 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan31 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan31 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan28 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan32 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 123 127 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan27 ].
			currentCharacter == $$ ifTrue: [ ^ self scan32 ].
			currentCharacter == $' ifTrue: [ ^ self scan33 ].
			currentCharacter == $\ ifTrue: [ ^ self scan38 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan26 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan33 [

	self recordMatch: #( 124 ).
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan34 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan35 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 127 ) ].
	^ self scan35X1
]

{ #category : #generated }
DartScanner >> scan35X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ 
				^ self recordAndReportMatch: #( 128 ) ].
			currentCharacter == $$ ifTrue: [ ^ self scan35 ].
			currentCharacter == $\ ifTrue: [ ^ self scan36 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan34 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan36 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan34 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan34 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan34 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan34 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan34 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan37 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan37 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan37 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan34 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan38 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan26 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan26 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan26 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan26 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan26 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan39 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan39 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan26 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan39 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan26 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan4 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ ^ self scan1 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan40 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan40X1 ].
	currentCharacter == $$ ifTrue: [ ^ self scan50 ].
	currentCharacter == $' ifTrue: [ ^ self scan51 ].
	currentCharacter == $\ ifTrue: [ ^ self scan52 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan44 ].
	^ self scan41
]

{ #category : #generated }
DartScanner >> scan40X1 [

	self recordMatch: #( 136 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan41 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan42 ].
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	currentCharacter == $| ifTrue: [ ^ self scan43 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan42 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan40 ].
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan43 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan28 ].
		currentCharacter == $$ ifTrue: [ ^ self scan50 ].
		currentCharacter == $' ifTrue: [ ^ self scan51 ].
		currentCharacter == $\ ifTrue: [ ^ self scan52 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan44 ].
		^ self scan41 ].
	currentCharacter == $$ ifTrue: [ ^ self scan29 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 124 ) ].
	currentCharacter == $\ ifTrue: [ ^ self scan30 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan28 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan44 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan45 ].
	currentCharacter == $| ifTrue: [ ^ self scan46 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan45 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			^ self recordAndReportMatch: #( 136 ) ].
		currentCharacter == $$ ifTrue: [ ^ self scan47 ].
		currentCharacter == $\ ifTrue: [ ^ self scan48 ].
		^ self scan44 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan46 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan47 ].
		currentCharacter == $\ ifTrue: [ ^ self scan48 ].
		(currentCharacter <= $! or: [ currentCharacter >= $# ]) ifTrue: [ 
			^ self scan44 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan47 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 135 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan45 ].
			currentCharacter == $| ifTrue: [ ^ self scan46 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan48 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan44 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan44 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan44 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan44 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan44 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan49 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan49 ].
	^ self scan44
]

{ #category : #generated }
DartScanner >> scan49 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan44 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan5 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan6 ].
	currentCharacter == $| ifTrue: [ ^ self scan7 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan50 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 123 135 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan42 ].
			currentCharacter == $$ ifTrue: [ ^ self scan29 ].
			currentCharacter == $' ifTrue: [ 
				^ self recordAndReportMatch: #( 124 ) ].
			currentCharacter == $\ ifTrue: [ ^ self scan30 ].
			currentCharacter == $| ifTrue: [ ^ self scan43 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan28 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan51 [

	self recordMatch: #( 124 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan45 ].
	currentCharacter == $| ifTrue: [ ^ self scan46 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan52 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan41 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan41 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan41 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan41 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan41 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan53 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan53 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan44 ].
	^ self scan41
]

{ #category : #generated }
DartScanner >> scan53 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan41 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan54 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan64 ].
	currentCharacter == $$ ifTrue: [ ^ self scan65 ].
	currentCharacter == $' ifTrue: [ ^ self scan54X1 ].
	currentCharacter == $\ ifTrue: [ ^ self scan66 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan58 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scan54X1 [

	self recordMatch: #( 132 ).
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan55 [

	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $' ifTrue: [ ^ self scan56 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	currentCharacter == $| ifTrue: [ ^ self scan57 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan56 [

	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $' ifTrue: [ ^ self scan54 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan57 [

	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 128 ) ].
	currentCharacter == $$ ifTrue: [ ^ self scan35 ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan64 ].
		currentCharacter == $$ ifTrue: [ ^ self scan65 ].
		currentCharacter == $' ifTrue: [ ^ self scan34 ].
		currentCharacter == $\ ifTrue: [ ^ self scan66 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan58 ].
		^ self scan55 ].
	currentCharacter == $\ ifTrue: [ ^ self scan36 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan58 [

	self step.
	currentCharacter == $' ifTrue: [ ^ self scan59 ].
	currentCharacter == $| ifTrue: [ ^ self scan60 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan59 [

	self step.
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan61 ].
		currentCharacter == $' ifTrue: [ 
			^ self recordAndReportMatch: #( 132 ) ].
		currentCharacter == $\ ifTrue: [ ^ self scan62 ].
		^ self scan58 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan6 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			^ self recordAndReportMatch: #( 133 ) ].
		currentCharacter == $$ ifTrue: [ ^ self scan8 ].
		currentCharacter == $\ ifTrue: [ ^ self scan9 ].
		^ self scan5 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan60 [

	self step.
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan61 ].
		currentCharacter == $\ ifTrue: [ ^ self scan62 ].
		(currentCharacter <= $& or: [ currentCharacter >= $( ]) ifTrue: [ 
			^ self scan58 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan61 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 131 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $' ifTrue: [ ^ self scan59 ].
			currentCharacter == $| ifTrue: [ ^ self scan60 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan62 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan58 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan58 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan58 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan58 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan58 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan63 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan63 ].
	^ self scan58
]

{ #category : #generated }
DartScanner >> scan63 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan58 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan64 [

	self recordMatch: #( 128 ).
	self step.
	currentCharacter == $' ifTrue: [ ^ self scan59 ].
	currentCharacter == $| ifTrue: [ ^ self scan60 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan65 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 127 131 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ 
				^ self recordAndReportMatch: #( 128 ) ].
			currentCharacter == $$ ifTrue: [ ^ self scan35 ].
			currentCharacter == $' ifTrue: [ ^ self scan56 ].
			currentCharacter == $\ ifTrue: [ ^ self scan36 ].
			currentCharacter == $| ifTrue: [ ^ self scan57 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan34 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan66 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan55 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan55 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan55 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan55 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan55 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan67 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan67 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan58 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scan67 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan55 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan68 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ 
		(self scanForString: '""') ifTrue: [ 
			[ 
			self recordMatch: #( 120 ).
			self step.
			currentCharacter == $" ] whileTrue ].
		^ self scan68 ].
	true ] whileTrue
]

{ #category : #generated }
DartScanner >> scan69 [

	[ 
	self step.
	currentCharacter == $' ifTrue: [ 
		(self scanForString: '''''') ifTrue: [ 
			[ 
			self recordMatch: #( 120 ).
			self step.
			currentCharacter == $' ] whileTrue ].
		^ self scan69 ].
	true ] whileTrue
]

{ #category : #generated }
DartScanner >> scan7 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan8 ].
		currentCharacter == $\ ifTrue: [ ^ self scan9 ].
		(currentCharacter <= $! or: [ currentCharacter >= $# ]) ifTrue: [ 
			^ self scan5 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan8 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 134 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan6 ].
			currentCharacter == $| ifTrue: [ ^ self scan7 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan9 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan5 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan5 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan5 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan5 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan5 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan10 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan10 ].
	^ self scan5
]

{ #category : #generated }
DartScanner >> scanForToken [

	self step.
	currentCharacter == $! ifTrue: [ ^ self scanForTokenX20 ].
	^ self scanForTokenX23
]

{ #category : #generated }
DartScanner >> scanForTokenX1 [

	self recordMatch: #( 140 ).
	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			self recordMatch: #( 119 ).
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan68 ].
			^ self reportLastMatch ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ 
				^ self recordAndReportMatch: #( 119 ) ].
			currentCharacter <= Character tab or: [ 
				(currentCharacter between: Character pageUp and: Character newPage) 
					or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue ].
		^ self reportLastMatch ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $' ifTrue: [ 
			self recordMatch: #( 119 ).
			self step.
			currentCharacter == $' ifTrue: [ ^ self scan69 ].
			^ self reportLastMatch ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			[ 
			self step.
			currentCharacter == $' ifTrue: [ 
				^ self recordAndReportMatch: #( 119 ) ].
			currentCharacter <= Character tab or: [ 
				(currentCharacter between: Character pageUp and: Character newPage) 
					or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue ].
		^ self reportLastMatch ].
	(currentCharacter == $$ or: [ 
		 (currentCharacter between: $0 and: $9) or: [ 
			 (currentCharacter between: $A and: $Z) or: [ 
				 currentCharacter == $_ or: [ 
					 currentCharacter between: $a and: $z ] ] ] ]) ifTrue: [ 
		[ 
		self recordMatch: #( 140 ).
		self step.
		currentCharacter == $$ or: [ 
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX10 [

	self recordMatch: #( 46 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 47 ) ].
	currentCharacter == $| ifTrue: [ 
		^ self recordAndReportMatch: #( 48 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX11 [

	self recordMatch: #( 32 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 33 ) ].
	currentCharacter == $> ifTrue: [ 
		^ self recordAndReportMatch: #( 34 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX12 [

	self recordMatch: #( 17 ).
	self step.
	currentCharacter == $- ifTrue: [ 
		^ self recordAndReportMatch: #( 18 ) ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 19 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX13 [

	self recordMatch: #( 13 ).
	self step.
	currentCharacter == $+ ifTrue: [ 
		^ self recordAndReportMatch: #( 14 ) ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 15 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX14 [

	self recordMatch: #( 6 ).
	self step.
	currentCharacter == $& ifTrue: [ ^ self recordAndReportMatch: #( 7 ) ].
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 8 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX15 [

	[ 
	self recordMatch: #( 52 ).
	self step.
	currentCharacter isSeparator or: [ 
		currentCharacter == Character pageUp ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX16 [

	self recordMatch: #( 44 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 45 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX17 [

	self recordMatch: #( 24 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 25 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX18 [

	self recordMatch: #( 11 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 12 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX19 [

	self recordMatch: #( 4 ).
	self step.
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 5 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX2 [

	self recordMatch: #( 117 ).
	self step.
	currentCharacter == $. ifTrue: [ ^ self scan23 ].
	currentCharacter isDigit ifTrue: [ ^ self scan24 ].
	('Ee' includes: currentCharacter) ifTrue: [ ^ self scan22 ].
	('Xx' includes: currentCharacter) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			[ 
			self recordMatch: #( 118 ).
			self step.
			currentCharacter isDigit or: [ 
				(currentCharacter between: $A and: $F) or: [ 
					currentCharacter between: $a and: $f ] ] ] whileTrue ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX20 [

	self recordMatch: #( 1 ).
	self step.
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 2 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX21 [

	currentCharacter == $r ifTrue: [ ^ self scanForTokenX1 ].
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 137 ) ].
	currentCharacter == $| ifTrue: [ ^ self scanForTokenX10 ].
	currentCharacter == $~ ifTrue: [ ^ self scanForTokenX9 ].
	currentCharacter = (Character value: 65279) ifTrue: [ 
		^ self recordAndReportMatch: #( 141 ) ].
	currentCharacter isDigit ifTrue: [ ^ self scan24 ].
	(currentCharacter isSeparator or: [ 
		 currentCharacter == Character pageUp ]) ifTrue: [ 
		^ self scanForTokenX15 ].
	((currentCharacter between: $$ and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ self scanForTokenX5 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX22 [

	currentCharacter == $/ ifTrue: [ ^ self scanForTokenX17 ].
	currentCharacter == $0 ifTrue: [ ^ self scanForTokenX2 ].
	currentCharacter == $: ifTrue: [ 
		^ self recordAndReportMatch: #( 26 ) ].
	currentCharacter == $; ifTrue: [ 
		^ self recordAndReportMatch: #( 27 ) ].
	currentCharacter == $< ifTrue: [ ^ self scanForTokenX8 ].
	currentCharacter == $= ifTrue: [ ^ self scanForTokenX11 ].
	currentCharacter == $> ifTrue: [ 
		^ self recordAndReportMatch: #( 35 ) ].
	currentCharacter == $? ifTrue: [ ^ self scan25 ].
	currentCharacter == $@ ifTrue: [ 
		^ self recordAndReportMatch: #( 41 ) ].
	currentCharacter == $[ ifTrue: [ 
		^ self recordAndReportMatch: #( 42 ) ].
	currentCharacter == $] ifTrue: [ 
		^ self recordAndReportMatch: #( 43 ) ].
	currentCharacter == $^ ifTrue: [ ^ self scanForTokenX16 ].
	^ self scanForTokenX21
]

{ #category : #generated }
DartScanner >> scanForTokenX23 [

	currentCharacter == $" ifTrue: [ ^ self scanForTokenX4 ].
	currentCharacter == $# ifTrue: [ ^ self scanForTokenX7 ].
	currentCharacter == $% ifTrue: [ ^ self scanForTokenX19 ].
	currentCharacter == $& ifTrue: [ ^ self scanForTokenX14 ].
	currentCharacter == $' ifTrue: [ ^ self scanForTokenX3 ].
	currentCharacter == $( ifTrue: [ ^ self recordAndReportMatch: #( 9 ) ].
	currentCharacter == $) ifTrue: [ 
		^ self recordAndReportMatch: #( 10 ) ].
	currentCharacter == $* ifTrue: [ ^ self scanForTokenX18 ].
	currentCharacter == $+ ifTrue: [ ^ self scanForTokenX13 ].
	currentCharacter == $, ifTrue: [ 
		^ self recordAndReportMatch: #( 16 ) ].
	currentCharacter == $- ifTrue: [ ^ self scanForTokenX12 ].
	currentCharacter == $. ifTrue: [ ^ self scanForTokenX6 ].
	^ self scanForTokenX22
]

{ #category : #generated }
DartScanner >> scanForTokenX3 [

	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan12 ].
	currentCharacter == $' ifTrue: [ 
		self recordMatch: #( 121 ).
		self step.
		currentCharacter == $' ifTrue: [ ^ self scan15 ].
		^ self reportLastMatch ].
	currentCharacter == $\ ifTrue: [ ^ self scan13 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan11 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX4 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self recordMatch: #( 125 ).
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan5 ].
		^ self reportLastMatch ].
	currentCharacter == $$ ifTrue: [ ^ self scan2 ].
	currentCharacter == $\ ifTrue: [ ^ self scan3 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan1 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX5 [

	[ 
	self recordMatch: #( 140 ).
	self step.
	currentCharacter == $$ or: [ 
		(currentCharacter between: $0 and: $9) or: [ 
			(currentCharacter between: $A and: $Z) or: [ 
				currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ] ] ] ]
		whileTrue
]

{ #category : #generated }
DartScanner >> scanForTokenX6 [

	self recordMatch: #( 20 ).
	self step.
	currentCharacter == $. ifTrue: [ 
		self recordMatch: #( 21 ).
		self step.
		currentCharacter == $. ifTrue: [ 
			self recordMatch: #( 22 ).
			self step.
			currentCharacter == $? ifTrue: [ 
				^ self recordAndReportMatch: #( 23 ) ] ].
		^ self reportLastMatch ].
	currentCharacter isDigit ifTrue: [ ^ self scan21 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX7 [

	self recordMatch: #( 3 ).
	self step.
	currentCharacter == $! ifTrue: [ 
		[ 
		self step.
		currentCharacter == Character lf ifTrue: [ 
			^ self recordAndReportMatch: #( 139 ) ].
		currentCharacter == Character cr ifTrue: [ 
			self recordMatch: #( 139 ).
			self step.
			currentCharacter == Character lf ifTrue: [ 
				^ self recordAndReportMatch: #( 139 ) ].
			^ self reportLastMatch ].
		true ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX8 [

	self recordMatch: #( 28 ).
	self step.
	currentCharacter == $< ifTrue: [ 
		self recordMatch: #( 29 ).
		self step.
		currentCharacter == $= ifTrue: [ 
			^ self recordAndReportMatch: #( 30 ) ].
		^ self reportLastMatch ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 31 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX9 [

	self recordMatch: #( 49 ).
	self step.
	currentCharacter == $/ ifTrue: [ 
		self recordMatch: #( 50 ).
		self step.
		currentCharacter == $= ifTrue: [ 
			^ self recordAndReportMatch: #( 51 ) ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> tokenActions [

	^ #( nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil #whitespace )
]
