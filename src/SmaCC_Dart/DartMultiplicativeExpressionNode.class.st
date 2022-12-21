Class {
	#name : #DartMultiplicativeExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'unaryExpressions',
		'multiplicativeOperators'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartMultiplicativeExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitMultiplicativeExpression: self
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> compositeNodeVariables [

	^ #( #unaryExpressions )
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> compositeTokenVariables [

	^ #( #multiplicativeOperators )
]

{ #category : #'generated-initialize-release' }
DartMultiplicativeExpressionNode >> initialize [
	super initialize.
	unaryExpressions := OrderedCollection new: 2.
	multiplicativeOperators := OrderedCollection new: 2.
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> multiplicativeOperators [

	^ multiplicativeOperators
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> multiplicativeOperators: anOrderedCollection [

	multiplicativeOperators := anOrderedCollection
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> unaryExpressions [

	^ unaryExpressions
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> unaryExpressions: anOrderedCollection [

	self setParents: self unaryExpressions to: nil.
	unaryExpressions := anOrderedCollection.
	self setParents: self unaryExpressions to: self
]
