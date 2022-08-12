Class {
	#name : #DartWhileStatementNode,
	#superclass : #DartStatementNode,
	#instVars : [
		'whileToken',
		'leftParenToken',
		'expression',
		'rightParenToken',
		'statement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartWhileStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitWhileStatement: self
]

{ #category : #generated }
DartWhileStatementNode >> expression [

	^ expression
]

{ #category : #generated }
DartWhileStatementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartWhileStatementNode >> leftParenToken [

	^ leftParenToken
]

{ #category : #generated }
DartWhileStatementNode >> leftParenToken: aSmaCCToken [

	leftParenToken := aSmaCCToken
]

{ #category : #generated }
DartWhileStatementNode >> otherVariables [

	^ #( #expression #statement )
]

{ #category : #generated }
DartWhileStatementNode >> rightParenToken [

	^ rightParenToken
]

{ #category : #generated }
DartWhileStatementNode >> rightParenToken: aSmaCCToken [

	rightParenToken := aSmaCCToken
]

{ #category : #generated }
DartWhileStatementNode >> statement [

	^ statement
]

{ #category : #generated }
DartWhileStatementNode >> statement: anOrderedCollection [

	self setParent: self statement to: nil.
	statement := anOrderedCollection.
	self setParent: self statement to: self
]

{ #category : #generated }
DartWhileStatementNode >> tokenVariables [

	^ #( #whileToken #leftParenToken #rightParenToken )
]

{ #category : #generated }
DartWhileStatementNode >> whileToken [

	^ whileToken
]

{ #category : #generated }
DartWhileStatementNode >> whileToken: aSmaCCToken [

	whileToken := aSmaCCToken
]
