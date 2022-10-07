Class {
	#name : #DartExpressionElementNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'expression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExpressionElementNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExpressionElement: self
]

{ #category : #generated }
DartExpressionElementNode >> expression [

	^ expression
]

{ #category : #generated }
DartExpressionElementNode >> expression: anOrderedCollection [

	self setParent: self expression to: nil.
	expression := anOrderedCollection.
	self setParent: self expression to: self
]

{ #category : #generated }
DartExpressionElementNode >> otherVariables [

	^ #( #expression )
]
