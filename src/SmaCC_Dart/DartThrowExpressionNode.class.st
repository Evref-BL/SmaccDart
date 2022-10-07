Class {
	#name : #DartThrowExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'throwExpression'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartThrowExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitThrowExpression: self
]

{ #category : #generated }
DartThrowExpressionNode >> otherVariables [

	^ #( #throwExpression )
]

{ #category : #generated }
DartThrowExpressionNode >> throwExpression [

	^ throwExpression
]

{ #category : #generated }
DartThrowExpressionNode >> throwExpression: anOrderedCollection [

	self setParent: self throwExpression to: nil.
	throwExpression := anOrderedCollection.
	self setParent: self throwExpression to: self
]
