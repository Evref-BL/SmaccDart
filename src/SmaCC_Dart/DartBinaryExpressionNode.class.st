Class {
	#name : #DartBinaryExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'logicalAndExpressions',
		'equalityExpressions',
		'relationalExpressions',
		'equalityOperator',
		'bitwiseOrExpressions',
		'typeTest',
		'typeCast',
		'relationalOperator',
		'bitwiseXorExpressions',
		'bitwiseAndExpressions',
		'shiftExpressions',
		'additiveExpressions',
		'shiftOperator'
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
DartBinaryExpressionNode >> equalityOperator [

	^ equalityOperator
]

{ #category : #generated }
DartBinaryExpressionNode >> equalityOperator: aSmaCCToken [

	equalityOperator := aSmaCCToken
]

{ #category : #'generated-initialize-release' }
DartBinaryExpressionNode >> initialize [
	super initialize.
	logicalAndExpressions := OrderedCollection new: 2.
	equalityExpressions := OrderedCollection new: 2.
	relationalExpressions := OrderedCollection new: 2.
	bitwiseOrExpressions := OrderedCollection new: 2.
	bitwiseXorExpressions := OrderedCollection new: 2.
	bitwiseAndExpressions := OrderedCollection new: 2.
	shiftExpressions := OrderedCollection new: 2.
	additiveExpressions := OrderedCollection new: 2.
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
DartBinaryExpressionNode >> relationalOperator [

	^ relationalOperator
]

{ #category : #generated }
DartBinaryExpressionNode >> relationalOperator: aSmaCCToken [

	relationalOperator := aSmaCCToken
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
DartBinaryExpressionNode >> shiftOperator [

	^ shiftOperator
]

{ #category : #generated }
DartBinaryExpressionNode >> shiftOperator: aSmaCCToken [

	shiftOperator := aSmaCCToken
]

{ #category : #generated }
DartBinaryExpressionNode >> tokenVariables [

	^ #( #equalityOperator #typeTest #typeCast #relationalOperator
	     #shiftOperator )
]

{ #category : #generated }
DartBinaryExpressionNode >> typeCast [

	^ typeCast
]

{ #category : #generated }
DartBinaryExpressionNode >> typeCast: aSmaCCToken [

	typeCast := aSmaCCToken
]

{ #category : #generated }
DartBinaryExpressionNode >> typeTest [

	^ typeTest
]

{ #category : #generated }
DartBinaryExpressionNode >> typeTest: aSmaCCToken [

	typeTest := aSmaCCToken
]
