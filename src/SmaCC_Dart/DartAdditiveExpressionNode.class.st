Class {
	#name : #DartAdditiveExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'multiplicativeExpressions',
		'additiveOperator'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAdditiveExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAdditiveExpression: self
]

{ #category : #generated }
DartAdditiveExpressionNode >> additiveOperator [

	^ additiveOperator
]

{ #category : #generated }
DartAdditiveExpressionNode >> additiveOperator: aSmaCCToken [

	additiveOperator := aSmaCCToken
]

{ #category : #generated }
DartAdditiveExpressionNode >> compositeNodeVariables [

	^ #( #multiplicativeExpressions )
]

{ #category : #'generated-initialize-release' }
DartAdditiveExpressionNode >> initialize [
	super initialize.
	multiplicativeExpressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartAdditiveExpressionNode >> multiplicativeExpressions [

	^ multiplicativeExpressions
]

{ #category : #generated }
DartAdditiveExpressionNode >> multiplicativeExpressions: anOrderedCollection [

	self setParents: self multiplicativeExpressions to: nil.
	multiplicativeExpressions := anOrderedCollection.
	self setParents: self multiplicativeExpressions to: self
]

{ #category : #generated }
DartAdditiveExpressionNode >> tokenVariables [

	^ #( #additiveOperator )
]
