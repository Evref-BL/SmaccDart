Class {
	#name : #DartMultiplicativeExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'unaryExpressions',
		'multiplicativeOperator'
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

{ #category : #'generated-initialize-release' }
DartMultiplicativeExpressionNode >> initialize [
	super initialize.
	unaryExpressions := OrderedCollection new: 2.
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> multiplicativeOperator [

	^ multiplicativeOperator
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> multiplicativeOperator: aSmaCCToken [

	multiplicativeOperator := aSmaCCToken
]

{ #category : #generated }
DartMultiplicativeExpressionNode >> tokenVariables [

	^ #( #multiplicativeOperator )
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
