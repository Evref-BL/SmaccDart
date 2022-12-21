Class {
	#name : #DartAdditiveExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'multiplicativeExpressions',
		'additiveOperators'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAdditiveExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAdditiveExpression: self
]

{ #category : #generated }
DartAdditiveExpressionNode >> additiveOperators [

	^ additiveOperators
]

{ #category : #generated }
DartAdditiveExpressionNode >> additiveOperators: anOrderedCollection [

	additiveOperators := anOrderedCollection
]

{ #category : #generated }
DartAdditiveExpressionNode >> compositeNodeVariables [

	^ #( #multiplicativeExpressions )
]

{ #category : #generated }
DartAdditiveExpressionNode >> compositeTokenVariables [

	^ #( #additiveOperators )
]

{ #category : #'generated-initialize-release' }
DartAdditiveExpressionNode >> initialize [
	super initialize.
	multiplicativeExpressions := OrderedCollection new: 2.
	additiveOperators := OrderedCollection new: 2.
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
