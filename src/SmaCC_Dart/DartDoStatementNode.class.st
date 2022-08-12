Class {
	#name : #DartDoStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'doToken',
		'statement',
		'whileToken',
		'leftParenToken',
		'expression',
		'rightParenToken',
		'semicolonToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDoStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDoStatement: self
]

{ #category : #generated }
DartDoStatementNode >> doToken [

	^ doToken
]

{ #category : #generated }
DartDoStatementNode >> doToken: aSmaCCToken [

	doToken := aSmaCCToken
]

{ #category : #generated }
DartDoStatementNode >> expression [

	^ expression
]

{ #category : #generated }
DartDoStatementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartDoStatementNode >> leftParenToken [

	^ leftParenToken
]

{ #category : #generated }
DartDoStatementNode >> leftParenToken: aSmaCCToken [

	leftParenToken := aSmaCCToken
]

{ #category : #generated }
DartDoStatementNode >> otherVariables [

	^ #( #statement #expression )
]

{ #category : #generated }
DartDoStatementNode >> rightParenToken [

	^ rightParenToken
]

{ #category : #generated }
DartDoStatementNode >> rightParenToken: aSmaCCToken [

	rightParenToken := aSmaCCToken
]

{ #category : #generated }
DartDoStatementNode >> semicolonToken [

	^ semicolonToken
]

{ #category : #generated }
DartDoStatementNode >> semicolonToken: aSmaCCToken [

	semicolonToken := aSmaCCToken
]

{ #category : #generated }
DartDoStatementNode >> statement [

	^ statement
]

{ #category : #generated }
DartDoStatementNode >> statement: anOrderedCollection [

	self setParent: self statement to: nil.
	statement := anOrderedCollection.
	self setParent: self statement to: self
]

{ #category : #generated }
DartDoStatementNode >> tokenVariables [

	^ #( #doToken #whileToken #leftParenToken #rightParenToken
	     #semicolonToken )
]

{ #category : #generated }
DartDoStatementNode >> whileToken [

	^ whileToken
]

{ #category : #generated }
DartDoStatementNode >> whileToken: aSmaCCToken [

	whileToken := aSmaCCToken
]
