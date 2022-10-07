Class {
	#name : #DartNormalParameterTypeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'typedIdentifier',
		'type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNormalParameterTypeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNormalParameterType: self
]

{ #category : #generated }
DartNormalParameterTypeNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartNormalParameterTypeNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartNormalParameterTypeNode >> nodeVariables [

	^ #( #typedIdentifier #type )
]

{ #category : #generated }
DartNormalParameterTypeNode >> otherVariables [

	^ #( #metadata )
]

{ #category : #generated }
DartNormalParameterTypeNode >> type [

	^ type
]

{ #category : #generated }
DartNormalParameterTypeNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]

{ #category : #generated }
DartNormalParameterTypeNode >> typedIdentifier [

	^ typedIdentifier
]

{ #category : #generated }
DartNormalParameterTypeNode >> typedIdentifier: aDartTypedIdentifierNode [

	self typedIdentifier notNil ifTrue: [ 
		self typedIdentifier parent: nil ].
	typedIdentifier := aDartTypedIdentifierNode.
	self typedIdentifier notNil ifTrue: [ 
		self typedIdentifier parent: self ]
]
