Class {
	#name : #DartBinaryExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'logicalAndExpressions',
		'equalityExpressions',
		'relationalExpressions',
		'equalityOperators',
		'bitwiseOrExpressions',
		'typeTests',
		'typeCasts',
		'relationalOperators',
		'bitwiseXorExpressions',
		'bitwiseAndExpressions',
		'shiftExpressions',
		'additiveExpressions',
		'shiftOperators'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartBinaryExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitBinaryExpression: self
]

{ #category : #generated }
DartBinaryExpressionNode >> additiveExpressions [

	^ additiveExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> additiveExpressions: anOrderedCollection [

	self setParents: self additiveExpressions to: nil.
	additiveExpressions := anOrderedCollection.
	self setParents: self additiveExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseAndExpressions [

	^ bitwiseAndExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseAndExpressions: anOrderedCollection [

	self setParents: self bitwiseAndExpressions to: nil.
	bitwiseAndExpressions := anOrderedCollection.
	self setParents: self bitwiseAndExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseOrExpressions [

	^ bitwiseOrExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseOrExpressions: anOrderedCollection [

	self setParents: self bitwiseOrExpressions to: nil.
	bitwiseOrExpressions := anOrderedCollection.
	self setParents: self bitwiseOrExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseXorExpressions [

	^ bitwiseXorExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> bitwiseXorExpressions: anOrderedCollection [

	self setParents: self bitwiseXorExpressions to: nil.
	bitwiseXorExpressions := anOrderedCollection.
	self setParents: self bitwiseXorExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> compositeNodeVariables [

	^ #( #logicalAndExpressions #equalityExpressions #relationalExpressions
	     #bitwiseOrExpressions #bitwiseXorExpressions
	     #bitwiseAndExpressions #shiftExpressions #additiveExpressions )
]

{ #category : #generated }
DartBinaryExpressionNode >> compositeTokenVariables [

	^ #( #equalityOperators #typeTests #typeCasts #relationalOperators
	     #shiftOperators )
]

{ #category : #generated }
DartBinaryExpressionNode >> equalityExpressions [

	^ equalityExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> equalityExpressions: anOrderedCollection [

	self setParents: self equalityExpressions to: nil.
	equalityExpressions := anOrderedCollection.
	self setParents: self equalityExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> equalityOperators [

	^ equalityOperators
]

{ #category : #generated }
DartBinaryExpressionNode >> equalityOperators: anOrderedCollection [

	equalityOperators := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartBinaryExpressionNode >> initialize [
	super initialize.
	logicalAndExpressions := OrderedCollection new: 2.
	equalityExpressions := OrderedCollection new: 2.
	relationalExpressions := OrderedCollection new: 2.
	equalityOperators := OrderedCollection new: 2.
	bitwiseOrExpressions := OrderedCollection new: 2.
	typeTests := OrderedCollection new: 2.
	typeCasts := OrderedCollection new: 2.
	relationalOperators := OrderedCollection new: 2.
	bitwiseXorExpressions := OrderedCollection new: 2.
	bitwiseAndExpressions := OrderedCollection new: 2.
	shiftExpressions := OrderedCollection new: 2.
	additiveExpressions := OrderedCollection new: 2.
	shiftOperators := OrderedCollection new: 2.
]

{ #category : #generated }
DartBinaryExpressionNode >> logicalAndExpressions [

	^ logicalAndExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> logicalAndExpressions: anOrderedCollection [

	self setParents: self logicalAndExpressions to: nil.
	logicalAndExpressions := anOrderedCollection.
	self setParents: self logicalAndExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> relationalExpressions [

	^ relationalExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> relationalExpressions: anOrderedCollection [

	self setParents: self relationalExpressions to: nil.
	relationalExpressions := anOrderedCollection.
	self setParents: self relationalExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> relationalOperators [

	^ relationalOperators
]

{ #category : #generated }
DartBinaryExpressionNode >> relationalOperators: anOrderedCollection [

	relationalOperators := anOrderedCollection
]

{ #category : #generated }
DartBinaryExpressionNode >> shiftExpressions [

	^ shiftExpressions
]

{ #category : #generated }
DartBinaryExpressionNode >> shiftExpressions: anOrderedCollection [

	self setParents: self shiftExpressions to: nil.
	shiftExpressions := anOrderedCollection.
	self setParents: self shiftExpressions to: self
]

{ #category : #generated }
DartBinaryExpressionNode >> shiftOperators [

	^ shiftOperators
]

{ #category : #generated }
DartBinaryExpressionNode >> shiftOperators: anOrderedCollection [

	shiftOperators := anOrderedCollection
]

{ #category : #generated }
DartBinaryExpressionNode >> typeCasts [

	^ typeCasts
]

{ #category : #generated }
DartBinaryExpressionNode >> typeCasts: anOrderedCollection [

	typeCasts := anOrderedCollection
]

{ #category : #generated }
DartBinaryExpressionNode >> typeTests [

	^ typeTests
]

{ #category : #generated }
DartBinaryExpressionNode >> typeTests: anOrderedCollection [

	typeTests := anOrderedCollection
]
