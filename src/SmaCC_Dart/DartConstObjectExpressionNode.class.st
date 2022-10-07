Class {
	#name : #DartConstObjectExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorDesignation',
		'arguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstObjectExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstObjectExpression: self
]

{ #category : #generated }
DartConstObjectExpressionNode >> arguments [

	^ arguments
]

{ #category : #generated }
DartConstObjectExpressionNode >> arguments: anObject [

	self setParent: self arguments to: nil.
	arguments := anObject.
	self setParent: self arguments to: self
]

{ #category : #generated }
DartConstObjectExpressionNode >> constructorDesignation [

	^ constructorDesignation
]

{ #category : #generated }
DartConstObjectExpressionNode >> constructorDesignation: anObject [

	self setParent: self constructorDesignation to: nil.
	constructorDesignation := anObject.
	self setParent: self constructorDesignation to: self
]

{ #category : #generated }
DartConstObjectExpressionNode >> otherVariables [

	^ #( #constructorDesignation #arguments )
]
