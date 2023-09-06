Class {
	#name : #DartSimpleFormalParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'declaredIdentifier',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSimpleFormalParameterNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSimpleFormalParameter: self
]

{ #category : #generated }
DartSimpleFormalParameterNode >> declaredIdentifier [

	^ declaredIdentifier
]

{ #category : #generated }
DartSimpleFormalParameterNode >> declaredIdentifier: aDartDeclaredIdentifierNode [

	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: nil ].
	declaredIdentifier := aDartDeclaredIdentifierNode.
	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: self ]
]

{ #category : #generated }
DartSimpleFormalParameterNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartSimpleFormalParameterNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartSimpleFormalParameterNode >> nodeVariables [

	^ #( #declaredIdentifier #identifier )
]
