Class {
	#name : #DartInitializerExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'conditionalExpression',
		'cascade'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartInitializerExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitInitializerExpression: self
]

{ #category : #generated }
DartInitializerExpressionNode >> cascade [

	^ cascade
]

{ #category : #generated }
DartInitializerExpressionNode >> cascade: aDartCascadeNode [

	self cascade notNil ifTrue: [ self cascade parent: nil ].
	cascade := aDartCascadeNode.
	self cascade notNil ifTrue: [ self cascade parent: self ]
]

{ #category : #generated }
DartInitializerExpressionNode >> conditionalExpression [

	^ conditionalExpression
]

{ #category : #generated }
DartInitializerExpressionNode >> conditionalExpression: aDartProgramNode [

	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: nil ].
	conditionalExpression := aDartProgramNode.
	self conditionalExpression notNil ifTrue: [ 
		self conditionalExpression parent: self ]
]

{ #category : #generated }
DartInitializerExpressionNode >> nodeVariables [

	^ #( #conditionalExpression #cascade )
]
