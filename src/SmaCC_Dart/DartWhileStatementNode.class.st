Class {
	#name : #DartWhileStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'whileConditionExpression',
		'whileStatement'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartWhileStatementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitWhileStatement: self
]

{ #category : #generated }
DartWhileStatementNode >> nodeVariables [

	^ #( #whileStatement )
]

{ #category : #generated }
DartWhileStatementNode >> otherVariables [

	^ #( #whileConditionExpression )
]

{ #category : #generated }
DartWhileStatementNode >> whileConditionExpression [

	^ whileConditionExpression
]

{ #category : #generated }
DartWhileStatementNode >> whileConditionExpression: anOrderedCollection [

	self setParent: self whileConditionExpression to: nil.
	whileConditionExpression := anOrderedCollection.
	self setParent: self whileConditionExpression to: self
]

{ #category : #generated }
DartWhileStatementNode >> whileStatement [

	^ whileStatement
]

{ #category : #generated }
DartWhileStatementNode >> whileStatement: aDartStatementsNode [

	self whileStatement notNil ifTrue: [ self whileStatement parent: nil ].
	whileStatement := aDartStatementsNode.
	self whileStatement notNil ifTrue: [ 
		self whileStatement parent: self ]
]
