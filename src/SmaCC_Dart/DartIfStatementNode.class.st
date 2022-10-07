Class {
	#name : #DartIfStatementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'ifConditionalExpression',
		'ifThenStatement',
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
DartIfStatementNode >> elseStatement: aDartSingleStatementNode [

	self elseStatement notNil ifTrue: [ self elseStatement parent: nil ].
	elseStatement := aDartSingleStatementNode.
	self elseStatement notNil ifTrue: [ self elseStatement parent: self ]
]

{ #category : #generated }
DartIfStatementNode >> ifConditionalExpression [

	^ ifConditionalExpression
]

{ #category : #generated }
DartIfStatementNode >> ifConditionalExpression: anOrderedCollection [

	self setParent: self ifConditionalExpression to: nil.
	ifConditionalExpression := anOrderedCollection.
	self setParent: self ifConditionalExpression to: self
]

{ #category : #generated }
DartIfStatementNode >> ifThenStatement [

	^ ifThenStatement
]

{ #category : #generated }
DartIfStatementNode >> ifThenStatement: aDartSingleStatementNode [

	self ifThenStatement notNil ifTrue: [ 
		self ifThenStatement parent: nil ].
	ifThenStatement := aDartSingleStatementNode.
	self ifThenStatement notNil ifTrue: [ 
		self ifThenStatement parent: self ]
]

{ #category : #generated }
DartIfStatementNode >> nodeVariables [

	^ #( #ifThenStatement #elseStatement )
]

{ #category : #generated }
DartIfStatementNode >> otherVariables [

	^ #( #ifConditionalExpression )
]
