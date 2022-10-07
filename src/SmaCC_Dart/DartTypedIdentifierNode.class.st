Class {
	#name : #DartTypedIdentifierNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'type',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypedIdentifierNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypedIdentifier: self
]

{ #category : #generated }
DartTypedIdentifierNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartTypedIdentifierNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartTypedIdentifierNode >> nodeVariables [

	^ #( #type #identifier )
]

{ #category : #generated }
DartTypedIdentifierNode >> type [

	^ type
]

{ #category : #generated }
DartTypedIdentifierNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
