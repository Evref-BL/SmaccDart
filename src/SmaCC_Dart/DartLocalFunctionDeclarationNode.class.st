Class {
	#name : #DartLocalFunctionDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'signature',
		'functionBody'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartLocalFunctionDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitLocalFunctionDeclaration: self
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionBody [

	^ functionBody
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> functionBody: aDartProgramNode [

	self functionBody notNil ifTrue: [ self functionBody parent: nil ].
	functionBody := aDartProgramNode.
	self functionBody notNil ifTrue: [ self functionBody parent: self ]
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> nodeVariables [

	^ #( #signature #functionBody )
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> otherVariables [

	^ #( #metadata )
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> signature [

	^ signature
]

{ #category : #generated }
DartLocalFunctionDeclarationNode >> signature: aDartFunctionSignatureNode [

	self signature notNil ifTrue: [ self signature parent: nil ].
	signature := aDartFunctionSignatureNode.
	self signature notNil ifTrue: [ self signature parent: self ]
]
