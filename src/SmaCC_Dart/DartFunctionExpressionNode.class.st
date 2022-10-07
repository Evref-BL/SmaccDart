Class {
	#name : #DartFunctionExpressionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'formalParameterPart',
		'functionExpressionBody'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionExpressionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionExpression: self
]

{ #category : #generated }
DartFunctionExpressionNode >> formalParameterPart [

	^ formalParameterPart
]

{ #category : #generated }
DartFunctionExpressionNode >> formalParameterPart: aDartFormalParameterPartNode [

	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: nil ].
	formalParameterPart := aDartFormalParameterPartNode.
	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: self ]
]

{ #category : #generated }
DartFunctionExpressionNode >> functionExpressionBody [

	^ functionExpressionBody
]

{ #category : #generated }
DartFunctionExpressionNode >> functionExpressionBody: anOrderedCollection [

	self setParent: self functionExpressionBody to: nil.
	functionExpressionBody := anOrderedCollection.
	self setParent: self functionExpressionBody to: self
]

{ #category : #generated }
DartFunctionExpressionNode >> nodeVariables [

	^ #( #formalParameterPart )
]

{ #category : #generated }
DartFunctionExpressionNode >> otherVariables [

	^ #( #functionExpressionBody )
]
