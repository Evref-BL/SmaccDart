Class {
	#name : #DartNamedParameterTypeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'typedIdentifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNamedParameterTypeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNamedParameterType: self
]

{ #category : #generated }
DartNamedParameterTypeNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartNamedParameterTypeNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartNamedParameterTypeNode >> nodeVariables [

	^ #( #typedIdentifier )
]

{ #category : #generated }
DartNamedParameterTypeNode >> otherVariables [

	^ #( #metadata )
]

{ #category : #generated }
DartNamedParameterTypeNode >> typedIdentifier [

	^ typedIdentifier
]

{ #category : #generated }
DartNamedParameterTypeNode >> typedIdentifier: aDartTypedIdentifierNode [

	self typedIdentifier notNil ifTrue: [ 
		self typedIdentifier parent: nil ].
	typedIdentifier := aDartTypedIdentifierNode.
	self typedIdentifier notNil ifTrue: [ 
		self typedIdentifier parent: self ]
]
