Class {
	#name : #DartFunctionDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'signature',
		'body'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionDeclaration: self
]

{ #category : #generated }
DartFunctionDeclarationNode >> body [

	^ body
]

{ #category : #generated }
DartFunctionDeclarationNode >> body: aDartProgramNode [

	self body notNil ifTrue: [ self body parent: nil ].
	body := aDartProgramNode.
	self body notNil ifTrue: [ self body parent: self ]
]

{ #category : #generated }
DartFunctionDeclarationNode >> nodeVariables [

	^ #( #signature #body )
]

{ #category : #generated }
DartFunctionDeclarationNode >> signature [

	^ signature
]

{ #category : #generated }
DartFunctionDeclarationNode >> signature: aDartProgramNode [

	self signature notNil ifTrue: [ self signature parent: nil ].
	signature := aDartProgramNode.
	self signature notNil ifTrue: [ self signature parent: self ]
]
