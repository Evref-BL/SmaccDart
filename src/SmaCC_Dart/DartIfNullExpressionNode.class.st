Class {
	#name : #DartIfNullExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'testNullExpression',
		'logicalOrExpressions'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartIfNullExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitIfNullExpression: self
]

{ #category : #generated }
DartIfNullExpressionNode >> compositeNodeVariables [

	^ #( #logicalOrExpressions )
]

{ #category : #'generated-initialize-release' }
DartIfNullExpressionNode >> initialize [
	super initialize.
	logicalOrExpressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartIfNullExpressionNode >> logicalOrExpressions [

	^ logicalOrExpressions
]

{ #category : #generated }
DartIfNullExpressionNode >> logicalOrExpressions: anOrderedCollection [

	self setParents: self logicalOrExpressions to: nil.
	logicalOrExpressions := anOrderedCollection.
	self setParents: self logicalOrExpressions to: self
]

{ #category : #generated }
DartIfNullExpressionNode >> nodeVariables [

	^ #( #testNullExpression )
]

{ #category : #generated }
DartIfNullExpressionNode >> testNullExpression [

	^ testNullExpression
]

{ #category : #generated }
DartIfNullExpressionNode >> testNullExpression: aDartProgramNode [

	self testNullExpression notNil ifTrue: [ 
		self testNullExpression parent: nil ].
	testNullExpression := aDartProgramNode.
	self testNullExpression notNil ifTrue: [ 
		self testNullExpression parent: self ]
]
