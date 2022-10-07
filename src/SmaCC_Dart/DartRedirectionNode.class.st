Class {
	#name : #DartRedirectionNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'arguments',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartRedirectionNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitRedirection: self
]

{ #category : #generated }
DartRedirectionNode >> arguments [

	^ arguments
]

{ #category : #generated }
DartRedirectionNode >> arguments: anObject [

	self setParent: self arguments to: nil.
	arguments := anObject.
	self setParent: self arguments to: self
]

{ #category : #generated }
DartRedirectionNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartRedirectionNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartRedirectionNode >> nodeVariables [

	^ #( #identifier )
]

{ #category : #generated }
DartRedirectionNode >> otherVariables [

	^ #( #arguments )
]
