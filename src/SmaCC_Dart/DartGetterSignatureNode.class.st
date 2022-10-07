Class {
	#name : #DartGetterSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartGetterSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitGetterSignature: self
]

{ #category : #generated }
DartGetterSignatureNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartGetterSignatureNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartGetterSignatureNode >> nodeVariables [

	^ #( #identifier #type )
]

{ #category : #generated }
DartGetterSignatureNode >> type [

	^ type
]

{ #category : #generated }
DartGetterSignatureNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
