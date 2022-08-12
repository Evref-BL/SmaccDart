Class {
	#name : #DartIfStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'ifToken',
		'leftParenToken',
		'expression',
		'rightParenToken',
		'thenStatement',
		'elseToken',
		'elseStatement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartIfStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitIfStatement: self
]

{ #category : #generated }
DartIfStatementNode >> elseStatement [

	^ elseStatement
]

{ #category : #generated }
DartIfStatementNode >> elseStatement: anOrderedCollection [

	self setParent: self elseStatement to: nil.
	elseStatement := anOrderedCollection.
	self setParent: self elseStatement to: self
]

{ #category : #generated }
DartIfStatementNode >> elseToken [

	^ elseToken
]

{ #category : #generated }
DartIfStatementNode >> elseToken: aSmaCCToken [

	elseToken := aSmaCCToken
]

{ #category : #generated }
DartIfStatementNode >> expression [

	^ expression
]

{ #category : #generated }
DartIfStatementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartIfStatementNode >> ifToken [

	^ ifToken
]

{ #category : #generated }
DartIfStatementNode >> ifToken: aSmaCCToken [

	ifToken := aSmaCCToken
]

{ #category : #generated }
DartIfStatementNode >> leftParenToken [

	^ leftParenToken
]

{ #category : #generated }
DartIfStatementNode >> leftParenToken: aSmaCCToken [

	leftParenToken := aSmaCCToken
]

{ #category : #generated }
DartIfStatementNode >> otherVariables [

	^ #( #expression #thenStatement #elseStatement )
]

{ #category : #generated }
DartIfStatementNode >> rightParenToken [

	^ rightParenToken
]

{ #category : #generated }
DartIfStatementNode >> rightParenToken: aSmaCCToken [

	rightParenToken := aSmaCCToken
]

{ #category : #generated }
DartIfStatementNode >> thenStatement [

	^ thenStatement
]

{ #category : #generated }
DartIfStatementNode >> thenStatement: anOrderedCollection [

	self setParent: self thenStatement to: nil.
	thenStatement := anOrderedCollection.
	self setParent: self thenStatement to: self
]

{ #category : #generated }
DartIfStatementNode >> tokenVariables [

	^ #( #ifToken #leftParenToken #rightParenToken #elseToken )
]
