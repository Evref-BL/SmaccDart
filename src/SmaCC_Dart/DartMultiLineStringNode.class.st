Class {
	#name : #DartMultiLineStringNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'firstExpression',
		'nextExpressionses'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMultiLineStringNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMultiLineString: self
]

{ #category : #generated }
DartMultiLineStringNode >> firstExpression [

	^ firstExpression
]

{ #category : #generated }
DartMultiLineStringNode >> firstExpression: anOrderedCollection [

	self setParent: self firstExpression to: nil.
	firstExpression := anOrderedCollection.
	self setParent: self firstExpression to: self
]

{ #category : #'generated-initialize-release' }
DartMultiLineStringNode >> initialize [
	super initialize.
	nextExpressionses := OrderedCollection new: 2.
]

{ #category : #generated }
DartMultiLineStringNode >> nextExpressionses [

	^ nextExpressionses
]

{ #category : #generated }
DartMultiLineStringNode >> nextExpressionses: anOrderedCollection [

	self setParent: self nextExpressionses to: nil.
	nextExpressionses := anOrderedCollection.
	self setParent: self nextExpressionses to: self
]

{ #category : #generated }
DartMultiLineStringNode >> otherVariables [

	^ #( #firstExpression #nextExpressionses )
]
