Class {
	#name : #DartFunctionPrefixNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'type',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionPrefixNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionPrefix: self
]

{ #category : #generated }
DartFunctionPrefixNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartFunctionPrefixNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartFunctionPrefixNode >> nodeVariables [

	^ #( #type #identifier )
]

{ #category : #generated }
DartFunctionPrefixNode >> type [

	^ type
]

{ #category : #generated }
DartFunctionPrefixNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
