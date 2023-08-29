Class {
	#name : #DartScanner,
	#superclass : #SmaCCScanner,
	#category : #'SmaCC_Dart'
}

{ #category : #'generated-initialization' }
DartScanner class >> initializeKeywordMap [

	keywordMap := Dictionary new.
	#( #( 137 'Function' 94 ) #( 137 'abstract' 85 )
	   #( 137 'as' 86 ) #( 137 'assert' 52 ) #( 137 'async' 110 )
	   #( 137 'await' 108 ) #( 137 'break' 53 ) #( 137 'case' 54 )
	   #( 137 'catch' 55 ) #( 137 'class' 56 ) #( 137 'const' 57 )
	   #( 137 'continue' 58 ) #( 137 'covariant' 87 )
	   #( 137 'default' 59 ) #( 137 'deferred' 88 )
	   #( 137 'do' 60 ) #( 137 'dynamic' 89 ) #( 137 'else' 61 )
	   #( 137 'enum' 62 ) #( 137 'export' 90 ) #( 137 'extends' 63 )
	   #( 137 'extension' 91 ) #( 137 'external' 92 )
	   #( 137 'factory' 93 ) #( 137 'false' 64 ) #( 137 'final' 65 )
	   #( 137 'finally' 66 ) #( 137 'for' 67 ) #( 137 'get' 95 )
	   #( 137 'hide' 111 ) #( 137 'if' 68 ) #( 137 'implements'
	      96 ) #( 137 'import' 97 ) #( 137 'in' 69 ) #( 137 'interface'
	      98 ) #( 137 'is' 70 ) #( 137 'late' 99 ) #( 137 'library'
	      100 ) #( 137 'mixin' 102 ) #( 137 'new' 71 ) #( 137 'null'
	      72 ) #( 137 'of' 112 ) #( 137 'on' 113 ) #( 137 'operator'
	      101 ) #( 137 'part' 103 ) #( 137 'required' 104 )
	   #( 137 'rethrow' 73 ) #( 137 'return' 74 ) #( 137 'set' 105 )
	   #( 137 'show' 114 ) #( 137 'static' 106 ) #( 137 'super' 75 )
	   #( 137 'switch' 76 ) #( 137 'sync' 115 ) #( 137 'this' 77 )
	   #( 137 'throw' 78 ) #( 137 'true' 79 ) #( 137 'try' 80 )
	   #( 137 'typedef' 107 ) #( 137 'var' 81 ) #( 137 'void' 82 )
	   #( 137 'while' 83 ) #( 137 'with' 84 ) #( 137 'yield' 109 ) ) do: [ 
		:each | 
		(keywordMap at: each first ifAbsentPut: [ Dictionary new ])
			at: (each at: 2)
			put: each last ].
	^ keywordMap
]

{ #category : #generated }
DartScanner >> emptySymbolTokenId [

	^ 456
]

{ #category : #generated }
DartScanner >> errorTokenId [

	^ 457
]

{ #category : #generated }
DartScanner >> scan1 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ 
		^ self recordAndReportMatch: #( 123 ) ].
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
DartScanner >> scan100 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan96 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan101 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan85 ].
			currentCharacter == $$ ifTrue: [ ^ self scan49 ].
			currentCharacter == $' ifTrue: [ ^ self scan86 ].
			currentCharacter == $\ ifTrue: [ ^ self scan50 ].
			currentCharacter == $| ifTrue: [ ^ self scan87 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan47 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan102 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan84 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan84 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan84 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan84 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan84 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan103 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan103 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
	^ self scan84
]

{ #category : #generated }
DartScanner >> scan103 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan84 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan11 [

	[ 
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan12 ].
	currentCharacter == $' ifTrue: [ 
		^ self recordAndReportMatch: #( 120 ) ].
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
		^ self recordAndReportMatch: #( 121 ) ].
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
				^ self recordAndReportMatch: #( 120 ) ].
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
			^ self recordAndReportMatch: #( 127 ) ].
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
		^ self recordAndReportMatch: #( 128 ) ].
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
		^ self recordAndReportMatch: #( 124 ) ].
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
	self recordMatch: #( 116 ).
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
		self recordMatch: #( 116 ).
		self step.
		currentCharacter isDigit ] whileTrue.
		^ self reportLastMatch ].
	('+-' includes: currentCharacter) ifTrue: [ 
		self step.
		currentCharacter isDigit ifTrue: [ 
			[ 
			self recordMatch: #( 116 ).
			self step.
			currentCharacter isDigit ] whileTrue ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan23 [

	[ 
	self step.
	currentCharacter == $* ifTrue: [ 
		[ 
		self step.
		currentCharacter == $* ] whileTrue.
		currentCharacter == $/ ifTrue: [ 
			^ self recordAndReportMatch: #( 140 ) ].
		^ self scan23 ].
	true ] whileTrue
]

{ #category : #generated }
DartScanner >> scan24 [

	self step.
	currentCharacter isDigit ifTrue: [ ^ self scan21 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan25 [

	[ 
	self recordMatch: #( 116 ).
	self step.
	currentCharacter == $. ifTrue: [ ^ self scan24 ].
	currentCharacter isDigit ] whileTrue.
	('Ee' includes: currentCharacter) ifTrue: [ ^ self scan22 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan26 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ 
		(self scanForString: '""') ifTrue: [ 
			[ 
			self recordMatch: #( 119 ).
			self step.
			currentCharacter == $" ] whileTrue ].
		^ self scan26 ].
	true ] whileTrue
]

{ #category : #generated }
DartScanner >> scan27 [

	[ 
	self step.
	currentCharacter == $' ifTrue: [ 
		(self scanForString: '''''') ifTrue: [ 
			[ 
			self recordMatch: #( 119 ).
			self step.
			currentCharacter == $' ] whileTrue ].
		^ self scan27 ].
	true ] whileTrue
]

{ #category : #generated }
DartScanner >> scan28 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			^ self recordAndReportMatch: #( 132 ) ].
		currentCharacter == $$ ifTrue: [ ^ self scan31 ].
		currentCharacter == $\ ifTrue: [ ^ self scan32 ].
		^ self scan29 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan29 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan28 ].
	currentCharacter == $| ifTrue: [ ^ self scan30 ].
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
				^ self recordAndReportMatch: #( 123 ) ].
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
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan31 ].
		currentCharacter == $\ ifTrue: [ ^ self scan32 ].
		(currentCharacter <= $! or: [ currentCharacter >= $# ]) ifTrue: [ 
			^ self scan29 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan31 [

	self step.
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan28 ].
			currentCharacter == $| ifTrue: [ ^ self scan30 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan32 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan29 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan29 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan29 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan29 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan29 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan33 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan33 ].
	^ self scan29
]

{ #category : #generated }
DartScanner >> scan33 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan29 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan34 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan35 ].
	currentCharacter == $$ ifTrue: [ ^ self scan45 ].
	currentCharacter == $' ifTrue: [ ^ self scan46 ].
	currentCharacter == $\ ifTrue: [ ^ self scan52 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan35 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ ^ self scan38 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan36 [

	[ 
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ ^ self scan38 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan37 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	^ self scan37X1
]

{ #category : #generated }
DartScanner >> scan37X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $$ ifTrue: [ ^ self scan37 ].
			currentCharacter == $' ifTrue: [ ^ self scan38 ].
			currentCharacter == $\ ifTrue: [ ^ self scan43 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan36 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan38 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan39 [

	[ 
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
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
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $$ ifTrue: [ ^ self scan40 ].
			currentCharacter == $\ ifTrue: [ ^ self scan41 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan39 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan41 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan39 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan39 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan39 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan39 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan39 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan42 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan42 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan42 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan39 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan43 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan36 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan36 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan36 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan36 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan36 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan44 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan44 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan44 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan36 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan45 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan35 ].
			currentCharacter == $$ ifTrue: [ ^ self scan45 ].
			currentCharacter == $' ifTrue: [ ^ self scan46 ].
			currentCharacter == $\ ifTrue: [ ^ self scan52 ].
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
DartScanner >> scan46 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan48 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan47 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan48 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	currentCharacter <= Character tab or: [ 
		(currentCharacter between: Character pageUp and: Character newPage) 
			or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan48 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan49 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	^ self scan49X1
]

{ #category : #generated }
DartScanner >> scan49X1 [

	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan48 ].
			currentCharacter == $$ ifTrue: [ ^ self scan49 ].
			currentCharacter == $\ ifTrue: [ ^ self scan50 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan47 ] ].
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
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan47 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan47 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan47 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan47 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan47 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan51 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan51 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan51 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan47 ] ].
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
				^ self scan53 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan53 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
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
			^ self scan34 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan54 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan36 ].
	currentCharacter == $$ ifTrue: [ ^ self scan79 ].
	currentCharacter == $' ifTrue: [ ^ self scan80 ].
	currentCharacter == $\ ifTrue: [ ^ self scan81 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scan55 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan56 ].
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ ^ self scan57 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	currentCharacter == $| ifTrue: [ ^ self scan78 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan56 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan54 ].
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ ^ self scan38 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan57 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan58 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan58 [

	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan77 ].
	currentCharacter == $' ifTrue: [ ^ self scan39 ].
	currentCharacter == $\ ifTrue: [ ^ self scan75 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan59
]

{ #category : #generated }
DartScanner >> scan59 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan60 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan62 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter == $| ifTrue: [ ^ self scan63 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan6 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			^ self recordAndReportMatch: #( 130 ) ].
		currentCharacter == $$ ifTrue: [ ^ self scan8 ].
		currentCharacter == $\ ifTrue: [ ^ self scan9 ].
		^ self scan5 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan60 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan61 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan61 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan39 ].
	currentCharacter == $$ ifTrue: [ ^ self scan73 ].
	currentCharacter == $\ ifTrue: [ ^ self scan75 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan59
]

{ #category : #generated }
DartScanner >> scan62 [

	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan58 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan63 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan61 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan58 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan64 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan65 ].
	currentCharacter == $' ifTrue: [ ^ self scan67 ].
	currentCharacter == $| ifTrue: [ ^ self scan69 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan65 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan66 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan66 [

	self step.
	currentCharacter == $$ ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			^ self recordAndReportMatch: #( 133 ) ].
		((currentCharacter between: $A and: $Z) or: [ 
			 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
			ifTrue: [ ^ self scan70 ].
		^ self reportLastMatch ].
	currentCharacter == $\ ifTrue: [ ^ self scan71 ].
	(currentCharacter <= $! or: [ currentCharacter >= $# ]) ifTrue: [ 
		^ self scan64 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan67 [

	self step.
	currentCharacter == $' ifTrue: [ ^ self scan68 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan68 [

	self step.
	currentCharacter == $$ ifTrue: [ 
		self step.
		((currentCharacter between: $A and: $Z) or: [ 
			 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
			ifTrue: [ ^ self scan70 ].
		^ self reportLastMatch ].
	currentCharacter == $\ ifTrue: [ ^ self scan71 ].
	(currentCharacter <= $& or: [ currentCharacter >= $( ]) ifTrue: [ 
		^ self scan64 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan69 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan66 ].
	currentCharacter == $' ifTrue: [ ^ self scan68 ].
	^ self reportLastMatch
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
DartScanner >> scan70 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan65 ].
	currentCharacter == $' ifTrue: [ ^ self scan67 ].
	currentCharacter == $| ifTrue: [ ^ self scan69 ].
	(currentCharacter between: $0 and: $9) or: [ 
		(currentCharacter between: $A and: $Z) or: [ 
			currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ] ] ]
		whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan71 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan64 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan64 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan64 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan64 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan64 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan72 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan72 ].
	^ self scan64
]

{ #category : #generated }
DartScanner >> scan72 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan64 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan73 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 133 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ ^ self scan74 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan74 [

	[ 
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan60 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan62 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter == $| ifTrue: [ ^ self scan63 ].
	(currentCharacter between: $0 and: $9) or: [ 
		(currentCharacter between: $A and: $Z) or: [ 
			currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ] ] ]
		whileTrue.
	^ self scan74X1
]

{ #category : #generated }
DartScanner >> scan74X1 [

	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan75 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan59 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan59 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan59 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan59 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan59 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan76 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan76 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan59
]

{ #category : #generated }
DartScanner >> scan76 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan59 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan77 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ ^ self scan74 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan78 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan54 ].
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ 
		self recordMatch: #( 122 ).
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan77 ].
		currentCharacter == $' ifTrue: [ ^ self scan39 ].
		currentCharacter == $\ ifTrue: [ ^ self scan75 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
		^ self scan59 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan79 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 133 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan56 ].
			currentCharacter == $$ ifTrue: [ ^ self scan37 ].
			currentCharacter == $' ifTrue: [ ^ self scan57 ].
			currentCharacter == $\ ifTrue: [ ^ self scan43 ].
			currentCharacter == $| ifTrue: [ ^ self scan78 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan36 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan8 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 131 ) ].
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
DartScanner >> scan80 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan60 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan62 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter == $| ifTrue: [ ^ self scan63 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan81 [

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
				^ self scan82 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan82 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scan82 [

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
DartScanner >> scan83 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan94 ].
	currentCharacter == $$ ifTrue: [ ^ self scan101 ].
	currentCharacter == $' ifTrue: [ 
		self recordMatch: #( 129 ).
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan48 ].
		currentCharacter == $$ ifTrue: [ ^ self scan49 ].
		currentCharacter == $\ ifTrue: [ ^ self scan50 ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			^ self scan47 ].
		^ self reportLastMatch ].
	currentCharacter == $\ ifTrue: [ ^ self scan102 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
	^ self scan84
]

{ #category : #generated }
DartScanner >> scan84 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan85 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $' ifTrue: [ ^ self scan86 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	currentCharacter == $| ifTrue: [ ^ self scan87 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan85 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan61 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan86 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan48 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $' ifTrue: [ ^ self scan83 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan87 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self recordMatch: #( 125 ).
		self step.
		currentCharacter == $" ifTrue: [ ^ self scan39 ].
		currentCharacter == $$ ifTrue: [ ^ self scan73 ].
		currentCharacter == $\ ifTrue: [ ^ self scan75 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
		^ self scan59 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $' ifTrue: [ ^ self scan87X1 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan87X1 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan94 ].
	currentCharacter == $$ ifTrue: [ ^ self scan101 ].
	currentCharacter == $' ifTrue: [ ^ self scan47 ].
	currentCharacter == $\ ifTrue: [ ^ self scan102 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
	^ self scan84
]

{ #category : #generated }
DartScanner >> scan88 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan65 ].
	currentCharacter == $' ifTrue: [ ^ self scan89 ].
	currentCharacter == $| ifTrue: [ ^ self scan90 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan89 [

	self step.
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan91 ].
		currentCharacter == $' ifTrue: [ 
			^ self recordAndReportMatch: #( 129 ) ].
		currentCharacter == $\ ifTrue: [ ^ self scan92 ].
		^ self scan88 ].
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
DartScanner >> scan90 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan66 ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan91 ].
		currentCharacter == $\ ifTrue: [ ^ self scan92 ].
		(currentCharacter <= $& or: [ currentCharacter >= $( ]) ifTrue: [ 
			^ self scan88 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan91 [

	self step.
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan65 ].
			currentCharacter == $' ifTrue: [ ^ self scan89 ].
			currentCharacter == $| ifTrue: [ ^ self scan90 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan92 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan88 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan88 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan88 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan88 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan88 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan93 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan93 ].
	^ self scan88
]

{ #category : #generated }
DartScanner >> scan93 [

	self step.
	(currentCharacter isDigit or: [ 
		 (currentCharacter between: $A and: $F) or: [ 
			 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			^ self scan88 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan94 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan60 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan95 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter == $| ifTrue: [ ^ self scan97 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan95 [

	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan98 ].
		currentCharacter == $' ifTrue: [ 
			self recordMatch: #( 129 ).
			self step.
			currentCharacter == $$ ifTrue: [ ^ self scan40 ].
			currentCharacter == $\ ifTrue: [ ^ self scan41 ].
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan39 ].
			^ self reportLastMatch ].
		currentCharacter == $\ ifTrue: [ ^ self scan99 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
		^ self scan96 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan96 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan60 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ ^ self scan95 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	currentCharacter == $| ifTrue: [ ^ self scan97 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan97 [

	self step.
	currentCharacter == $" ifTrue: [ ^ self scan61 ].
	currentCharacter == $$ ifTrue: [ ^ self scan40 ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $$ ifTrue: [ ^ self scan98 ].
		currentCharacter == $' ifTrue: [ ^ self scan39 ].
		currentCharacter == $\ ifTrue: [ ^ self scan99 ].
		(currentCharacter == Character lf or: [ 
			 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
		^ self scan96 ].
	currentCharacter == $\ ifTrue: [ ^ self scan41 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan39 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan98 [

	self step.
	currentCharacter == ${ ifTrue: [ 
		^ self recordAndReportMatch: #( 126 ) ].
	((currentCharacter between: $A and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan60 ].
			currentCharacter == $$ ifTrue: [ ^ self scan40 ].
			currentCharacter == $' ifTrue: [ ^ self scan95 ].
			currentCharacter == $\ ifTrue: [ ^ self scan41 ].
			currentCharacter == $| ifTrue: [ ^ self scan97 ].
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] whileTrue.
			(currentCharacter <= Character tab or: [ 
				 (currentCharacter
					  between: Character pageUp
					  and: Character newPage) or: [ 
					 currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
				^ self scan39 ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scan99 [

	self step.
	currentCharacter == $u ifTrue: [ 
		self step.
		currentCharacter == ${ ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				self step.
				currentCharacter == $} ifTrue: [ ^ self scan96 ].
				(currentCharacter isDigit or: [ 
					 (currentCharacter between: $A and: $F) or: [ 
						 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
					self step.
					currentCharacter == $} ifTrue: [ ^ self scan96 ].
					(currentCharacter isDigit or: [ 
						 (currentCharacter between: $A and: $F) or: [ 
							 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
						self step.
						currentCharacter == $} ifTrue: [ ^ self scan96 ].
						(currentCharacter isDigit or: [ 
							 (currentCharacter between: $A and: $F) or: [ 
								 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
							self step.
							currentCharacter == $} ifTrue: [ ^ self scan96 ].
							(currentCharacter isDigit or: [ 
								 (currentCharacter between: $A and: $F) or: [ 
									 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
								self step.
								(currentCharacter isDigit or: [ 
									 (currentCharacter between: $A and: $F) or: [ 
										 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
									self step ].
								currentCharacter == $} ifTrue: [ ^ self scan96 ] ] ] ] ] ].
			^ self reportLastMatch ].
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			self step.
			(currentCharacter isDigit or: [ 
				 (currentCharacter between: $A and: $F) or: [ 
					 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
				^ self scan100 ] ].
		^ self reportLastMatch ].
	currentCharacter == $x ifTrue: [ ^ self scan100 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
	^ self scan96
]

{ #category : #generated }
DartScanner >> scanForToken [

	self step.
	currentCharacter == $! ifTrue: [ ^ self scanForTokenX23 ].
	currentCharacter == $" ifTrue: [ ^ self scanForTokenX6 ].
	^ self scanForTokenX26
]

{ #category : #generated }
DartScanner >> scanForTokenX1 [

	self recordMatch: #( 135 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scanForTokenX1X2 ].
	currentCharacter == $$ ifTrue: [ ^ self scan45 ].
	currentCharacter == $' ifTrue: [ ^ self scanForTokenX1X1 ].
	currentCharacter == $\ ifTrue: [ ^ self scan52 ].
	currentCharacter == $| ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ ^ self scanForTokenX1X4 ].
		currentCharacter == $$ ifTrue: [ ^ self scan45 ].
		currentCharacter == $' ifTrue: [ ^ self scanForTokenX1X3 ].
		currentCharacter == $\ ifTrue: [ ^ self scan52 ] ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan34 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX10 [

	self recordMatch: #( 3 ).
	self step.
	currentCharacter == $! ifTrue: [ 
		[ 
		self step.
		currentCharacter == Character lf ifTrue: [ 
			^ self recordAndReportMatch: #( 136 ) ].
		currentCharacter == Character cr ifTrue: [ 
			self recordMatch: #( 136 ).
			self step.
			currentCharacter == Character lf ifTrue: [ 
				^ self recordAndReportMatch: #( 136 ) ].
			^ self reportLastMatch ].
		true ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX11 [

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
DartScanner >> scanForTokenX12 [

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
DartScanner >> scanForTokenX13 [

	self recordMatch: #( 46 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 47 ) ].
	currentCharacter == $| ifTrue: [ 
		^ self recordAndReportMatch: #( 48 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX14 [

	self recordMatch: #( 32 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 33 ) ].
	currentCharacter == $> ifTrue: [ 
		^ self recordAndReportMatch: #( 34 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX15 [

	self recordMatch: #( 17 ).
	self step.
	currentCharacter == $- ifTrue: [ 
		^ self recordAndReportMatch: #( 18 ) ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 19 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX16 [

	self recordMatch: #( 13 ).
	self step.
	currentCharacter == $+ ifTrue: [ 
		^ self recordAndReportMatch: #( 14 ) ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 15 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX17 [

	self recordMatch: #( 6 ).
	self step.
	currentCharacter == $& ifTrue: [ ^ self recordAndReportMatch: #( 7 ) ].
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 8 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX18 [

	self recordMatch: #( 134 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan28 ].
	currentCharacter == $| ifTrue: [ ^ self scan30 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX19 [

	[ 
	self recordMatch: #( 139 ).
	self step.
	currentCharacter isSeparator or: [ 
		currentCharacter == Character pageUp ] ] whileTrue.
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX1X1 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan48 ].
	currentCharacter == $$ ifTrue: [ ^ self scan49 ].
	currentCharacter == $' ifTrue: [ ^ self scan83 ].
	currentCharacter == $\ ifTrue: [ ^ self scan50 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan47 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX1X2 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan54 ].
	currentCharacter == $$ ifTrue: [ ^ self scan37 ].
	currentCharacter == $' ifTrue: [ ^ self scan38 ].
	currentCharacter == $\ ifTrue: [ ^ self scan43 ].
	(currentCharacter <= Character tab or: [ 
		 (currentCharacter between: Character pageUp and: Character newPage) 
			 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
		^ self scan36 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX1X3 [

	self recordMatch: #( 122 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan94 ].
	currentCharacter == $$ ifTrue: [ ^ self scan101 ].
	currentCharacter == $' ifTrue: [ ^ self scan47 ].
	currentCharacter == $\ ifTrue: [ ^ self scan102 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan88 ].
	^ self scan84
]

{ #category : #generated }
DartScanner >> scanForTokenX1X4 [

	self recordMatch: #( 125 ).
	self step.
	currentCharacter == $" ifTrue: [ ^ self scan36 ].
	currentCharacter == $$ ifTrue: [ ^ self scan79 ].
	currentCharacter == $' ifTrue: [ ^ self scan80 ].
	currentCharacter == $\ ifTrue: [ ^ self scan81 ].
	(currentCharacter == Character lf or: [ 
		 currentCharacter == Character cr ]) ifTrue: [ ^ self scan64 ].
	^ self scan55
]

{ #category : #generated }
DartScanner >> scanForTokenX2 [

	self recordMatch: #( 137 ).
	self step.
	currentCharacter == $" ifTrue: [ 
		self step.
		currentCharacter == $" ifTrue: [ 
			self recordMatch: #( 118 ).
			self step.
			currentCharacter == $" ifTrue: [ ^ self scan26 ].
			^ self reportLastMatch ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			[ 
			self step.
			currentCharacter == $" ifTrue: [ 
				^ self recordAndReportMatch: #( 118 ) ].
			currentCharacter <= Character tab or: [ 
				(currentCharacter between: Character pageUp and: Character newPage) 
					or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue ].
		^ self reportLastMatch ].
	currentCharacter == $' ifTrue: [ 
		self step.
		currentCharacter == $' ifTrue: [ 
			self recordMatch: #( 118 ).
			self step.
			currentCharacter == $' ifTrue: [ ^ self scan27 ].
			^ self reportLastMatch ].
		(currentCharacter <= Character tab or: [ 
			 (currentCharacter between: Character pageUp and: Character newPage) 
				 or: [ currentCharacter >= (Character value: 14) ] ]) ifTrue: [ 
			[ 
			self step.
			currentCharacter == $' ifTrue: [ 
				^ self recordAndReportMatch: #( 118 ) ].
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
		self recordMatch: #( 137 ).
		self step.
		currentCharacter == $$ or: [ 
			(currentCharacter between: $0 and: $9) or: [ 
				(currentCharacter between: $A and: $Z) or: [ 
					currentCharacter == $_ or: [ 
						currentCharacter between: $a and: $z ] ] ] ] ] whileTrue ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX20 [

	self recordMatch: #( 44 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 45 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX21 [

	self recordMatch: #( 11 ).
	self step.
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 12 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX22 [

	self recordMatch: #( 4 ).
	self step.
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 5 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX23 [

	self recordMatch: #( 1 ).
	self step.
	currentCharacter == $= ifTrue: [ ^ self recordAndReportMatch: #( 2 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX24 [

	currentCharacter == ${ ifTrue: [ ^ self scanForTokenX18 ].
	currentCharacter == $| ifTrue: [ ^ self scanForTokenX13 ].
	currentCharacter == $} ifTrue: [ ^ self scanForTokenX1 ].
	currentCharacter == $~ ifTrue: [ ^ self scanForTokenX12 ].
	currentCharacter = (Character value: 65279) ifTrue: [ 
		^ self recordAndReportMatch: #( 138 ) ].
	currentCharacter isDigit ifTrue: [ ^ self scan25 ].
	(currentCharacter isSeparator or: [ 
		 currentCharacter == Character pageUp ]) ifTrue: [ 
		^ self scanForTokenX19 ].
	((currentCharacter between: $$ and: $Z) or: [ 
		 currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ]) 
		ifTrue: [ self scanForTokenX7 ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX25 [

	currentCharacter == $0 ifTrue: [ ^ self scanForTokenX4 ].
	currentCharacter == $: ifTrue: [ 
		^ self recordAndReportMatch: #( 26 ) ].
	currentCharacter == $; ifTrue: [ 
		^ self recordAndReportMatch: #( 27 ) ].
	currentCharacter == $< ifTrue: [ ^ self scanForTokenX11 ].
	currentCharacter == $= ifTrue: [ ^ self scanForTokenX14 ].
	currentCharacter == $> ifTrue: [ 
		^ self recordAndReportMatch: #( 35 ) ].
	currentCharacter == $? ifTrue: [ ^ self scanForTokenX8 ].
	currentCharacter == $@ ifTrue: [ 
		^ self recordAndReportMatch: #( 41 ) ].
	currentCharacter == $[ ifTrue: [ 
		^ self recordAndReportMatch: #( 42 ) ].
	currentCharacter == $] ifTrue: [ 
		^ self recordAndReportMatch: #( 43 ) ].
	currentCharacter == $^ ifTrue: [ ^ self scanForTokenX20 ].
	currentCharacter == $r ifTrue: [ ^ self scanForTokenX2 ].
	^ self scanForTokenX24
]

{ #category : #generated }
DartScanner >> scanForTokenX26 [

	currentCharacter == $# ifTrue: [ ^ self scanForTokenX10 ].
	currentCharacter == $% ifTrue: [ ^ self scanForTokenX22 ].
	currentCharacter == $& ifTrue: [ ^ self scanForTokenX17 ].
	currentCharacter == $' ifTrue: [ ^ self scanForTokenX5 ].
	currentCharacter == $( ifTrue: [ ^ self recordAndReportMatch: #( 9 ) ].
	currentCharacter == $) ifTrue: [ 
		^ self recordAndReportMatch: #( 10 ) ].
	currentCharacter == $* ifTrue: [ ^ self scanForTokenX21 ].
	currentCharacter == $+ ifTrue: [ ^ self scanForTokenX16 ].
	currentCharacter == $, ifTrue: [ 
		^ self recordAndReportMatch: #( 16 ) ].
	currentCharacter == $- ifTrue: [ ^ self scanForTokenX15 ].
	currentCharacter == $. ifTrue: [ ^ self scanForTokenX9 ].
	currentCharacter == $/ ifTrue: [ ^ self scanForTokenX3 ].
	^ self scanForTokenX25
]

{ #category : #generated }
DartScanner >> scanForTokenX3 [

	self recordMatch: #( 24 ).
	self step.
	currentCharacter == $* ifTrue: [ ^ self scan23 ].
	currentCharacter == $/ ifTrue: [ 
		[ 
		self recordMatch: #( 140 ).
		self step.
		currentCharacter <= Character tab or: [ 
			(currentCharacter between: Character pageUp and: Character newPage) 
				or: [ currentCharacter >= (Character value: 14) ] ] ] whileTrue.
		^ self reportLastMatch ].
	currentCharacter == $= ifTrue: [ 
		^ self recordAndReportMatch: #( 25 ) ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX4 [

	self recordMatch: #( 116 ).
	self step.
	currentCharacter == $. ifTrue: [ ^ self scan24 ].
	currentCharacter isDigit ifTrue: [ ^ self scan25 ].
	('Ee' includes: currentCharacter) ifTrue: [ ^ self scan22 ].
	('Xx' includes: currentCharacter) ifTrue: [ 
		self step.
		(currentCharacter isDigit or: [ 
			 (currentCharacter between: $A and: $F) or: [ 
				 currentCharacter between: $a and: $f ] ]) ifTrue: [ 
			[ 
			self recordMatch: #( 117 ).
			self step.
			currentCharacter isDigit or: [ 
				(currentCharacter between: $A and: $F) or: [ 
					currentCharacter between: $a and: $f ] ] ] whileTrue ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX5 [

	self step.
	currentCharacter == $$ ifTrue: [ ^ self scan12 ].
	currentCharacter == $' ifTrue: [ 
		self recordMatch: #( 120 ).
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
DartScanner >> scanForTokenX6 [

	self step.
	currentCharacter == $" ifTrue: [ 
		self recordMatch: #( 123 ).
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
DartScanner >> scanForTokenX7 [

	[ 
	self recordMatch: #( 137 ).
	self step.
	currentCharacter == $$ or: [ 
		(currentCharacter between: $0 and: $9) or: [ 
			(currentCharacter between: $A and: $Z) or: [ 
				currentCharacter == $_ or: [ currentCharacter between: $a and: $z ] ] ] ] ]
		whileTrue
]

{ #category : #generated }
DartScanner >> scanForTokenX8 [

	self recordMatch: #( 36 ).
	self step.
	currentCharacter == $. ifTrue: [ 
		self recordMatch: #( 37 ).
		self step.
		currentCharacter == $. ifTrue: [ 
			^ self recordAndReportMatch: #( 38 ) ].
		^ self reportLastMatch ].
	currentCharacter == $? ifTrue: [ 
		self recordMatch: #( 39 ).
		self step.
		currentCharacter == $= ifTrue: [ 
			^ self recordAndReportMatch: #( 40 ) ] ].
	^ self reportLastMatch
]

{ #category : #generated }
DartScanner >> scanForTokenX9 [

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
DartScanner >> tokenActions [

	^ #( nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil nil nil nil #whitespace #comment )
]
