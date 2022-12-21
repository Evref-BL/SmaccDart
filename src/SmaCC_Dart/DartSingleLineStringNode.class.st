Class {
	#name : #DartSingleLineStringNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'firstExpression',
		'nextExpressionses'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSingleLineStringNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSingleLineString: self
]

{ #category : #generated }
DartSingleLineStringNode >> firstExpression [

	^ firstExpression
]

{ #category : #generated }
DartSingleLineStringNode >> firstExpression: anOrderedCollection [

	self setParent: self firstExpression to: nil.
	firstExpression := anOrderedCollection.
	self setParent: self firstExpression to: self
]

{ #category : #'generated-initialize-release' }
DartSingleLineStringNode >> initialize [
	super initialize.
	nextExpressionses := OrderedCollection new: 2.
]

{ #category : #generated }
DartSingleLineStringNode >> nextExpressionses [

	^ nextExpressionses
]

{ #category : #generated }
DartSingleLineStringNode >> nextExpressionses: anOrderedCollection [

	self setParent: self nextExpressionses to: nil.
	nextExpressionses := anOrderedCollection.
	self setParent: self nextExpressionses to: self
]

{ #category : #generated }
DartSingleLineStringNode >> otherVariables [

	^ #( #firstExpression #nextExpressionses )
]
