Class {
	#name : #DartFieldInitializerNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'initializerExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFieldInitializerNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFieldInitializer: self
]

{ #category : #generated }
DartFieldInitializerNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartFieldInitializerNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartFieldInitializerNode >> initializerExpression [

	^ initializerExpression
]

{ #category : #generated }
DartFieldInitializerNode >> initializerExpression: aDartInitializerExpressionNode [

	self initializerExpression notNil ifTrue: [ 
		self initializerExpression parent: nil ].
	initializerExpression := aDartInitializerExpressionNode.
	self initializerExpression notNil ifTrue: [ 
		self initializerExpression parent: self ]
]

{ #category : #generated }
DartFieldInitializerNode >> nodeVariables [

	^ #( #identifier #initializerExpression )
]
