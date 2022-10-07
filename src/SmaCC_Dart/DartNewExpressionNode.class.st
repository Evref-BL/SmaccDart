Class {
	#name : #DartNewExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'constructorDesignation',
		'constructorArguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNewExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNewExpression: self
]

{ #category : #generated }
DartNewExpressionNode >> constructorArguments [

	^ constructorArguments
]

{ #category : #generated }
DartNewExpressionNode >> constructorArguments: anObject [

	self setParent: self constructorArguments to: nil.
	constructorArguments := anObject.
	self setParent: self constructorArguments to: self
]

{ #category : #generated }
DartNewExpressionNode >> constructorDesignation [

	^ constructorDesignation
]

{ #category : #generated }
DartNewExpressionNode >> constructorDesignation: anObject [

	self setParent: self constructorDesignation to: nil.
	constructorDesignation := anObject.
	self setParent: self constructorDesignation to: self
]

{ #category : #generated }
DartNewExpressionNode >> otherVariables [

	^ #( #constructorDesignation #constructorArguments )
]
