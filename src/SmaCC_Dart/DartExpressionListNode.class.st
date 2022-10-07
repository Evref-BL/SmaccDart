Class {
	#name : #DartExpressionListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'firstExpression',
		'nextExpressions'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExpressionListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExpressionList: self
]

{ #category : #generated }
DartExpressionListNode >> firstExpression [

	^ firstExpression
]

{ #category : #generated }
DartExpressionListNode >> firstExpression: anOrderedCollection [

	self setParent: self firstExpression to: nil.
	firstExpression := anOrderedCollection.
	self setParent: self firstExpression to: self
]

{ #category : #'generated-initialize-release' }
DartExpressionListNode >> initialize [
	super initialize.
	nextExpressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartExpressionListNode >> nextExpressions [

	^ nextExpressions
]

{ #category : #generated }
DartExpressionListNode >> nextExpressions: anOrderedCollection [

	self setParent: self nextExpressions to: nil.
	nextExpressions := anOrderedCollection.
	self setParent: self nextExpressions to: self
]

{ #category : #generated }
DartExpressionListNode >> otherVariables [

	^ #( #firstExpression #nextExpressions )
]
